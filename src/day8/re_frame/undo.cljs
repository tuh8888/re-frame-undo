(ns day8.re-frame.undo
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as reagent]
            [re-frame.core :as re-frame]
            [re-frame.db :refer [app-db]]))


;; -- Configuration ----------------------------------------------------------

(def config
  (atom {:max-undos         50 ;; Maximum number of undo states maintained
         :harvest-fn        deref
         :reinstate-fn      reset!
         :undo-list         (reagent/atom {})
         :redo-list         (reagent/atom {})
         ;; -- Explanations
         ;; -----------------------------------------------------------
         ;;
         ;; Each undo has an associated string explanation, for display to the
         ;; user.
         ;;
         ;; It seems ugly to have mirrored vectors, but ...
         ;; the code kinda falls out when you do. I'm feeling lazy.
         :undo-explain-list (reagent/atom {}) ;"Mirrors undo-list"
         :app-explain       (reagent/atom "") ;"Mirrors app-db"
         :redo-explain-list (reagent/atom []) ;"Mirrors redo-list"
        }))

(defn undo-config!
  "Set configuration parameters for library.

  Should be called on app startup."
  [new-config]
  (if-let [unknown-keys (seq (clojure.set/difference (-> new-config
                                                         keys
                                                         set)
                                                     (-> @config
                                                         keys
                                                         set)))]
    (re-frame/console :error
                      "re-frame-undo: undo-config! called within unknown keys: "
                      unknown-keys)
    (swap! config merge new-config)))


(defn max-undos [] (:max-undos @config))

(defn clear-undos!
  []
  (reset! (:undo-list @config) {})
  (reset! (:undo-explain-list @config) {}))


(defn clear-redos!
  []
  (reset! (:redo-list @config) {})
  (reset! (:redo-explain-list @config) {}))


(defn clear-history!
  []
  (clear-undos!)
  (clear-redos!)
  (reset! (:app-explain @config) ""))


(defn store-now!
  "Stores the value currently in app-db, so the user can later undo"
  [explanation]
  (clear-redos!)
  (swap! (:undo-explain-list @config)
    (fn [undo-explain-list-map]
      (update undo-explain-list-map
              app-db
              (fn [undo-explain-list]
                (->> @(:app-explain @config)
                     (conj undo-explain-list)
                     (take-last (max-undos))
                     vec)))))
  (swap! (:undo-list @config)
    (fn [undo-list-map]
      (update undo-list-map
              app-db
              (fn [undo-list]
                (->> app-db
                     ((:harvest-fn @config))
                     (conj undo-list)
                     (take-last (max-undos))
                     vec)))))
  (reset! (:app-explain @config) explanation))


(defn undos?
  "Returns true if undos exist, false otherwise"
  []
  (seq (get @(:undo-list @config) app-db)))

(defn redos?
  "Returns true if redos exist, false otherwise"
  []
  (seq (get @(:redo-list @config) app-db)))

(defn undo-explanations
  "Returns a vector of undo descriptions, perhaps empty"
  []
  (if (undos?)
    (conj (get @(:undo-explain-list @config) app-db) @(:app-explain @config))
    []))

;; -- subscriptions
;; -----------------------------------------------------------------------------

(re-frame/reg-sub-raw :undos?                   ;;  usage:  (subscribe
  ;;  [:undos?])
  (fn handler
    ; "returns true if anything is stored in the undo list, otherwise false"
    [_ _]
    (reaction (undos?))))

(re-frame/reg-sub-raw :redos?
  (fn handler
    ; "returns true if anything is stored in the redo list, otherwise false"
    [_ _]
    (reaction (redos?))))


(re-frame/reg-sub-raw :undo-explanations
  (fn handler
    ; "returns a vector of string explanations ordered oldest to most recent"
    [_ _]
    (reaction (undo-explanations))))

(re-frame/reg-sub-raw :redo-explanations
  (fn handler
    ; "returns a vector of string explanations ordered from most recent undo
    ; onward"
    [_ _]
    (reaction (deref (:redo-explain-list @config)))))

;; -- event handlers
;; ----------------------------------------------------------------------------


(defn undo
  [harvester reinstater undos cur redos]
  (let [u (get @undos app-db)
        r (cons (harvester cur) (get @redos app-db))]
    (reinstater cur (last u))
    (swap! redos assoc app-db r)
    (swap! undos assoc app-db (pop u))))


(defn undo-n
  "undo n steps or until we run out of undos"
  [n]
  (when (and (pos? n) (undos?))
    (undo (:harvest-fn @config)
          (:reinstate-fn @config)
          (:undo-list @config)
          app-db
          (:redo-list @config))
    (undo deref
          reset!
          (:undo-explain-list @config)
          (:app-explain @config)
          (:redo-explain-list @config))
    (recur (dec n))))

(defn undo-handler
  [_ [_ n]]
  (if-not (undos?)
    (re-frame/console
     :warn
     "re-frame: you did a (dispatch [:undo]), but there is nothing to undo.")
    (undo-n (or n 1)))
  {}); work is done directly on app-db

(defn redo
  [harvester reinstater undos cur redos]
  (let [u (conj (get @undos app-db) (harvester cur))
        r (get @redos app-db)]
    (reinstater cur (first r))
    (swap! redos assoc app-db (rest r))
    (swap! undos assoc app-db u)))

(defn redo-n
  "redo n steps or until we run out of redos"
  [n]
  (when (and (pos? n) (redos?))
    (redo (:harvest-fn @config)
          (:reinstate-fn @config)
          (:undo-list @config)
          app-db
          (:redo-list @config))
    (redo deref
          reset!
          (:undo-explain-list @config)
          (:app-explain @config)
          (:redo-explain-list @config))
    (recur (dec n))))

(defn redo-handler
  [_ [_ n]]  ;; if n absent, defaults to 1
  (if-not (redos?)
    (re-frame/console
     :warn
     "re-frame: you did a (dispatch [:redo]), but there is nothing to redo.")
    (redo-n (or n 1)))
  {}); work is done directly on app-db

(defn purge-redo-handler
  [db _]
  (if-not (redos?)
    (re-frame/console
     :warn
     "re-frame: you did a (dispatch [:purge-redos]), but there is nothing to redo.")
    (clear-redos!))
  db)


;; -- Interceptors ----------------------------------------------------------

(defn undoable
  "returns a side-effecting Interceptor, which stores an undo checkpoint in
  `:after` processing.
   If the `:effect` cotnains an `:undo` key, then use the explanation provided
   by it. Otherwise, `explanation` can be:
     - a string (of explanation)
     - a function expected to return a string of explanation. It will be called
       with two arguments: `db` and `event-vec`.
     - a nil, in which case \"\" is recorded as the explanation
  "
  ([] (undoable nil))
  ([explanation]
   (re-frame/->interceptor
    :id :undoable
    :after
    (fn [context]
      (let [event (re-frame/get-coeffect context :event)
            undo-effect (re-frame/get-effect context :undo)
            explanation
            (cond (some? undo-effect) undo-effect
                  (fn? explanation)
                  (explanation (re-frame/get-coeffect context :db) event)
                  (string? explanation) explanation
                  (nil? explanation) ""
                  :else (re-frame/console
                         :error
                         "re-frame-undo: \"undoable\" interceptor on event "
                         event
                         " given a bad parameter. Got: " explanation))]
        (store-now! explanation)
        (update context :effects dissoc :undo))))));; remove any `:undo` effect. Already handled.


;; -- register handlers for events and subscriptions


(defn register-events-subs!
  []
  (re-frame/reg-event-fx :undo
    ;; usage:  (dispatch [:undo n])  n is optional, defaults to 1
    undo-handler)
  (re-frame/reg-event-fx :redo
    ;; usage:  (dispatch [:redo n])
    redo-handler)
  (re-frame/reg-event-db :purge-redos
    ;; usage:  (dispatch [:purge-redos])
    purge-redo-handler))

(register-events-subs!)
