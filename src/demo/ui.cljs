(ns demo.ui
 (:require
    [clojure.core :as core]
    [reagent.core :as r]
    [clojure.string :as string]
    [sci.core :as sci]
    [react-flow-renderer :default ReactFlow]
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

(defn new-label [state]
  (->> (state :steps)
       (map (fn [s] (clojure.string/replace (s :label) #"\$" "")))
       (apply max)))
       


#_(insert-before
  [{:label "A"
    :code "[10 15 26]"}
   {:label "B"
    :code "(map inc A)"}
   {:label "C"
    :code "(reduce + B)"}]
   "B"
   {:label (new-label)
    :code "(fn [i] i)"})

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

(defn normalize-label
  "Returns normalize label or nil if can't normalize"
  [s]
  (let [s (-> s
              (string/upper-case)
              (string/replace #"[^A-Z]" ""))]
   (when-not (string/blank? s)
    (str "$" s))))

#_(normalize-label "e % 2")

(defn label-exists? [steps label]
  (some (fn [step] (= (step :label) label)) steps))

(defn rename-step! [old-label new-label]
  (when-let [new-label (normalize-label new-label)]
   (swap! state update :steps
     (fn [steps]
       (if (label-exists? steps new-label)
        steps
        (map (fn [step]
              (-> step
                  (update :code (fn [code] (string/replace code #"\$[A-Z]+"
                                                           (fn [label]
                                                             (if (= label old-label)
                                                              new-label
                                                              label)))))
                  (update :label (fn [label] (if (= label old-label)
                                              new-label
                                              label)))))
             steps))))))

(defn generate-new-label [steps]
  (let [letters (mapv char (range (.charCodeAt "A") (.charCodeAt "Z")))
        new-label (apply str "$" (repeatedly 3 #(rand-nth letters)))]
    (if (label-exists? steps new-label)
     (recur steps)
     new-label)))

(defn insert-step-before! [label]
  (swap! state update :steps
    (fn [steps]
      (let [[before after] (split-with (fn [step] (not= (step :label) label)) steps)]
       (concat before
               [{:label (generate-new-label steps)
                 :code "nil"}]
               after)))))

(defn remove-step! [label]
  (swap! state update :steps (fn [steps] (remove (fn [step] (= (step :label) label)) steps))))

(defn edit-step-code! [label code]
  (swap! state update :steps (fn [steps]
                               (map (fn [step]
                                     (if (= (:label step)
                                            label)
                                      (assoc step :code code)
                                      step))
                                    steps))))


(defn analyze
  "For each step, identifies which steps it depends on"
  [steps]
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
  (pr steps)
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


(def elements
  [{
    :id "1"
    :type "input"
    :data { :label "Input Node" }
    :position { :x 250 :y 25 }
   }
   {
    :id "2"
    :data { :label "Default Node" }
    :position { :x 100 :y 125 }
   }
   {
    :id "3"
    :type "output"
    :data { :label "Output Node" }
    :position { :x 250 :y 250 }
   }
   { :id "e1-2" :source "1" :target "2" :animated true }
   { :id "e2-3" :source "2" :target "3" }
   ])

(defn app-view []
  [:div {:style {:height 600 :border "solid 1px #DDDDDD"}}
   [:> ReactFlow {:elements elements}]
   ])

#_(defn app-view []
  [:div
   [:button {:on-click (fn [] (re-order!))} "Re-order"]
   (let [results (calculate-results! (@state :steps))]
    [:table
     [:tbody
      (for [{:keys [label code]} (:steps @state)
            :let [result (get results (symbol label) ::NO-RESULT)]]
        ^{:key label}
        [:<>
         [:tr
          [:td
           [:button {:on-click (fn [_] (insert-step-before! label))} "+"]]]
         [:tr.step
          [:td [:button {:on-click (fn [] (rename-step! label (js/prompt "What to rename?")))} label]]
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
           [:button {:on-click (fn [_] (remove-step! label))} "x"]]]])
      [:tr
       [:td
        [:button {:on-click (fn [_] (insert-step-before! nil))} "+"]]]]])])

;; change :label and :code to :step/label and :step/code
;; maybe explore using specter
;; code autoformatting
;; uploading a csv
