(ns demo.ui
 (:require
    [reagent.core :as r]
    [sci.core :as sci]
    [demo.toposort :as toposort]))

(defn remove-from-vector [vector label]
  (filter (fn [step]
            (not= (step :label) label)) vector))

#_(remove-from-vector 
    [{:label "A"
      :code "[10 15 26]"}
     {:label "B"
      :code "(map inc A)"}
     {:label "C"
      :code "(reduce + B)"}] "B")

(defn insert-before [coll label v]
  (let [[before after]
        (split-with (comp (complement #{label}) :label) coll)]
    (-> (vec before)
        (conj v)
        (into after))))

(defn new-label []
  "X")

#_(insert-before
  [{:label "A"
    :code "[10 15 26]"}
   {:label "B"
    :code "(map inc A)"}
   {:label "C"
    :code "(reduce + B)"}]
  "B"
  {
   :label (new-label)
   :code "(fn [i] i)"
  })

(defonce state
  (r/atom
    {:steps [{:label "$A"
              :code "[10 15 26]"}
             {:label "$B"
              :code "(map inc $A)"}
             {:label "$C"
              :code "(reduce + $B)"}
             {:label "$D"
              :code "(count $A)"}
             {:label "$E"
              :code "(/ $C $D)"}]}))

(defn insert-step-before! [label]
  (swap! state update :steps insert-before label {:code "(fn [i] i)"}))

(defn remove-step! [label]
  (swap! state update :steps remove-from-vector label))

(defn edit-step-code! [label code]
  (swap! state update :steps (fn [steps]
                               (map (fn [step]
                                     (if (= (:label step)
                                            label)
                                      (assoc step :code code)
                                      step))
                                    steps))))


(defn analyze [steps]
  (->> steps
       (map (fn [step]
              [(:label step) (set (re-seq #"\$[A-Z]+" (:code step)))]))
       (into {})))

#_(toposort/toposort (analyze [{:label "$A"
                                :code "[10 15 26]"}
                               {:label "$B"
                                :code "(map inc $A)"}
                               {:label "$C"
                                :code "(reduce + $B)"}
                               {:label "$E"
                                :code "(/ $C $D)"}
                               {:label "$D"
                                :code "(count $A)"}]))

#_{"$A" #{}
   "$B" #{"$A"}
   "$C" #{"$B"}
   "$D" #{"$A"}
   "$E" #{"$C" "$D"}}


(defn analyze-and-reorder [steps]
  (let [labels-in-order (toposort/toposort (analyze steps))
        label->step (zipmap (map :label steps)
                            steps)]
    (map (fn [l] (label->step l)) labels-in-order)))

#_(analyze-and-reorder [{:label "$A"
                         :code "[10 15 26]"}
                        {:label "$B"
                         :code "(map inc $A)"}
                        {:label "$C"
                         :code "(reduce + $B)"}
                        {:label "$E"
                         :code "(/ $C $D)"}
                        {:label "$D"
                         :code "(count $A)"}])

(defn re-order! []
  (swap! state update :steps analyze-and-reorder))

(defn calculate-results! [steps]
  (try
   (loop [context {}
          remaining-steps (analyze-and-reorder steps)]
    (let [{:keys [label code]} (first remaining-steps)
          result (try
                   (sci/eval-string code {:namespaces {'user context}})
                   (catch js/Error e
                     e))]
     (cond
       (= (type result) ExceptionInfo)
       (assoc context (symbol label) result)

       (seq remaining-steps)
       (recur (assoc context (symbol label) result)
              (rest remaining-steps))

       :done
       context)))
   (catch js/Error e
     {})))

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
  [:div
   [:button {:on-click (fn [] (re-order!))} "Re-order"]
   (let [results (calculate-results! (@state :steps))]
    [:table
     [:tbody
      [:tr
       [:td
        [:button {:on-click (fn [_] (insert-step-before! 0))} "+"]]]
      (for [{:keys [label code]} (:steps @state)
            :let [result (get results (symbol label) ::NO-RESULT)]]
        ^{:key label}
        [:<>
         [:tr.step
          [:td label]
          [:td
           [:textarea {:value code
                       :on-change (fn [e]
                                    (edit-step-code! label (.. e -target -value)))}]]
          [:td
           [:span "=>"]]
          [:td
           [:span {}
             (cond
              (= (type result) ExceptionInfo)
              (.-message result)
              (= result ::NO-RESULT)
              ""
              :else
              (pr-str result))]]
          [:td
           [:button {:on-click (fn [_] (remove-step! label))} "x"]]]
         [:tr
          [:td
           [:button {:on-click (fn [_] #_(insert-step-before! (inc index)))} "+"]]]])]])])

;; temporarily disabling step
;; moving a step
;; code autoformatting
;; uploading a csv
