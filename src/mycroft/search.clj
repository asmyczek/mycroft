(ns mycroft.search
  (:require [clojure.contrib [str-utils :as utils]]
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
  (if (empty? query)
    {}
    (case mode
      "is"      (is query)
      "no-doc"  (find-vars query false)
      (find-vars query))))

(def #^{:private true}
  hl-list 
  (apply concat (repeat ["<span class=\"hlsearch\">" "</span>"])))

(defn- hl-search
  "Highlight search string."
  [pattern text]
  (let [mtc (re-matcher pattern text)]
    (if (. mtc find)
      (let [fpos (. mtc start)
            pseq (utils/re-partition pattern text)]
        (apply str (interleave pseq hl-list)))
      text)))

(defn- render-var 
  "Render var including documentation."
  [ns v pattern] 
  [:li 
   (breadcrumb/link-to v #(hl-search pattern (str %)))
   [:pre (hl-search pattern (docs/doc-string v))]])

(defn- render-namespace
  "Render namespace."
  [[ns vars] pattern]
  [:li 
   [:div (breadcrumb/link-to ns #(hl-search pattern (str %)))]
   [:ul (map #(render-var ns % pattern) vars)]])

(defn search-layout
  "Search page layout."
  [query results] 
  (let [ns-groups (group-by (fn [v] (-> v meta :ns)) results)] 
    [:div
      [:p "Search results for '" [:b query] "' in "
       [:span {:class "buttons"} 
        [:a {:href (str "/search?mode=no-doc&query=" query)} "Names"]
        [:a {:href (str "/search?mode=with-doc&query=" query)} "Documentation"]]]
      [:p [:ul (map #(render-namespace % (re-pattern query)) ns-groups)]]]))

