(ns mycroft.search
  (:require [clojure.contrib [str-utils :as utils]]
            [mycroft.breadcrumb :as breadcrumb]
            [mycroft.namespace :as nspc]
            [mycroft.docs :as docs]
            [ring.util.response :as resp]))

(defn- find-vars 
  "Find vars for matching regex pattern and condition functions.
  'cond' takes the var as arguments and returns true or false."
  ([cond] 
   (for [ns (nspc/namespaces) vs (nspc/vars (str ns)) :when (cond vs)]
     vs)))

(defn- matches
  "Condition function that matches patter in var name or documentation."
  [pattern in-doc]
  (fn [var]
    (let [name (-> var meta :name str)
          doc  (-> var meta :doc)] 
      (or (re-find (re-matcher pattern name)) 
          (and in-doc doc (re-find (re-matcher pattern doc)))))))

(defn- name-is
  [query]
  (find-vars #(= query (-> % meta :name str))))

(defn- name-contains
  [pattern]
  (find-vars (matches pattern false)))

(defn- var-contains
  [pattern]
  (find-vars (matches pattern true)))

(defn search 
  "Main search entry point. Returns a list of matching vars."
  [query mode]
  (if (empty? query) {}
    (let [pattern (re-pattern query)]
      (case mode
        "is"      (let [vars (name-is query)] 
                    (if (empty? vars) 
                      (name-contains pattern) 
                      vars))
        "no-doc"  (name-contains pattern)
        (var-contains pattern)))))

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

(defn- group-by-ns
  "Group vars by namespace."
  [vars] 
  (sort-by 
    (comp str val) 
    (group-by (fn [v] (-> v meta :ns)) vars)))

(defn search-layout
  "Search page layout."
  [query results] 
  (let [ns-groups (group-by-ns results)]
    [:div
      [:p "Search results for '" [:b query] "' in "
       [:span {:class "buttons"} 
        [:a {:href (str "/search?mode=no-doc&query=" query)} "Names"]
        [:a {:href (str "/search?mode=with-doc&query=" query)} "Documentation"]]]
      [:p [:ul (map #(render-namespace % (re-pattern query)) ns-groups)]]]))

