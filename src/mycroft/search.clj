(ns mycroft.search
  (:require [clojure.contrib [str-utils2 :as utils]]
            [mycroft.breadcrumb :as breadcrumb]
            [mycroft.docs :as docs]
            [ring.util.response :as resp])
  (:import [java.util.regex Pattern]))

(defn- find-vars 
  "'find-doc' rip-off for now.
  If in-doc is true, search in doc string as well."
  ([query]      (find-vars query true))
  ([query in-doc] 
   (let [re  (re-pattern query)] 
     (for [ns (all-ns) 
           v (sort-by (comp :name meta) (vals (ns-interns ns))) 
           :when (and (:doc (meta v)) 
                      (or (re-find (re-matcher re (str (:name (meta v))))) 
                          (and in-doc (re-find (re-matcher re (:doc (meta v)))))))]
       v))))

(defn- is
  "Search for exact name."
  [query] 
  (let [vars (for [ns (all-ns) 
               v (sort-by (comp :name meta) (vals (ns-interns ns))) 
               :when (= query (-> v meta :name str))] v)]
    (if (empty? vars)
      (find-vars query false) 
      vars)))

(defn search 
  "Main search entry point. Returns a list of matching vars."
  [query mode]
  (case mode
    "is"      (is query)
    "no-doc"  (find-vars query false)
    (find-vars query)))

(defn- render-var 
  "Render var including documentation."
  [ns v query] 
  [:li 
   (breadcrumb/link-to v)
   [:pre (docs/doc-string v)]])

(defn- render-namespace
  "Render namespace."
  [[ns vars] query]
  [:li 
   [:div (breadcrumb/link-to ns)]
   [:ul (map #(render-var ns % query) vars)]])

(defn search-layout
  "Search page layout."
  [query results] 
  (let [ns-groups (group-by (fn [v] (-> v meta :ns)) results)] 
    [:div
      [:p "Search results for '" [:b query] "' in "
       [:span {:class "buttons"} 
        [:a {:href (str "/search?mode=no-doc&query=" query)} "Names"]
        [:a {:href (str "/search?mode=with-doc&query=" query)} "Documentation"]]]
      [:p [:ul (map #(render-namespace % query) ns-groups)]]]))

