(ns demo.ui
 (:require
    [reagent.core :as r]
    [sci.core :as sci]))

(defn remove-from-vector [vector i]
  (vec (concat (subvec vector 0 i)
               (subvec vector (inc i)))))

#_(remove-from-vector [:a :b :c :d] 2)

(defn insert-in-vector [vector i value]
  (vec (concat (subvec vector 0 i)
               [value]
               (subvec vector i))))


#_(insert-in-vector [:a :b :c] 0 :new)

(defonce state
  (r/atom
    {:steps [{:label "A"
              :code "[10 15 26]"}
             {:label "B"
              :code "(map inc A)"}
             {:label "C"
              :code "(reduce + B)"}
             {:label "D"
              :code "(count A)"}
             {:label "E"
              :code "(/ C D)"}]}))

(defn insert-step-before! [i]
  (swap! state update :steps insert-in-vector i {:code "(fn [i] i)"}))

(defn remove-step! [i]
  (swap! state update :steps remove-from-vector i))

(defn edit-step-code! [label code]
  (swap! state update :steps (fn [steps]
                               (map (fn [step]
                                     (if (= (:label step)
                                            label)
                                      (assoc step :code code)
                                      step))
                                    steps))))

#_(defn calculate-results! []
     (try
       (vec (reductions (fn [memo f]
                         (f memo))
                (sci/eval-string (:initial-input @state))
                (map (fn [step]
                       (sci/eval-string (:code step)))
                     (:steps @state))))
       (catch js/Error e
         [])))


(defn calculate-results! [steps]
  (loop [context {}
         remaining-steps steps]
   (let [{:keys [label code]} (first remaining-steps)
         result (try
                  (sci/eval-string code {:namespaces {'user context}})
                  (catch js/Error e
                    e))]
    (println (type result))
    (cond
      (= (type result) ExceptionInfo)
      (assoc context (symbol label) result)
      (seq remaining-steps)
      (recur (assoc context (symbol label) result)
             (rest remaining-steps))
      :done
      context))))


#_(let [steps [{:label "A"
                :code "[10 15 26]"}
               {:label "B"
                :code "(map inc A)"}
               {:label "C"
                :code "(reduce + B)"}
               {:label "D"
                :code "(count A)"}
               {:label "E"
                :code "(/ C D)"}]]
    (calculate-results! steps))



#_(sci/eval-string "(+ A 3)" {:namespaces {'user {'A 7}}})
#_(calculate-results!)

(defn app-view []
  (let [results (calculate-results! (@state :steps))]
   [:div
    [:div
     [:button {:on-click (fn [_] (insert-step-before! 0))} "+"]]
    (for [{:keys [label code]} (:steps @state)
          :let [result (get results (symbol label) ::NO-RESULT)]]
      ^{:key label}
      [:<>
       [:div.step
        [:textarea {:value code
                    :on-change (fn [e]
                                 (edit-step-code! label (.. e -target -value)))}]
        [:span label "=>"]
        [:span {}
          (cond
           (= (type result) ExceptionInfo)
           (.-message result)
           (= result ::NO-RESULT)
           ""
           :else
           (pr-str result))]
        [:button {:on-click (fn [_] #_(remove-step! index))} "x"]]
       [:div
        [:button {:on-click (fn [_] #_(insert-step-before! (inc index)))} "+"]]])]))


;; temporarily disabling step
;; moving a step
;; code autoformatting
;; uploading a csv
