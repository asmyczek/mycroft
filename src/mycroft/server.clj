(ns mycroft.server
  (:use [ring.adapter.jetty :only (run-jetty)]
        [ring.util.response :only (redirect)]
        [compojure.core :only (defroutes GET POST routes)]
        [hiccup.core :only (html)]
        [hiccup.page-helpers :only [include-js include-css]]
        [clojure.pprint :only (pprint)]
        [clojure.walk :only (keywordize-keys)]
        [clojure.java.browse :only (browse-url)])
  (:require
   [compojure.route :as route]
   [mycroft.jmx :as jmx]
   [mycroft.breadcrumb :as breadcrumb]
   [mycroft.resources :as resources]
   [mycroft.data :as data]
   [mycroft.class :as class]
   [mycroft.namespace :as namespace]
   [mycroft.docs :as docs]
   [mycroft.search :as search]
   [mycroft.history :as history]
   [clojure.string :as str]
   [mycroft.examples :as examples]))

(defn- minib-layout [title query & body] 
  (html
    [:head
     [:title title]
     (include-css "/stylesheets/shCore.css"
                  "/stylesheets/shThemeDefault.css")
                  [:link {:type "text/css", :href "/stylesheets/mobile.css", :rel "stylesheet", :media ""}]
     [:meta {:name "viewport" :content "user-scalable=no, width=device-width"}]
     (include-js "/javascripts/jquery.1.3.2.min.js"
                 "/javascripts/application.js"
                 "/javascripts/shCore.js"
                 "/javascripts/shBrushClojure.js")]
    [:body {:id "browser"}
     [:h2 {:class "logo"} 
      [:span {:class "search"}
        [:form {:method "GET" :action "/search"}
         [:input {:type "text", :name "query", :value query }] 
         [:input {:type "hidden", :name "mode", :value "with-doc"}]
         [:input {:type "submit" :value "Search"}]]]
      [:a {:href "/" :class "home"} "Home"]
      [:a {:href "/index.html"} "Mycroft, a Clojure inspector"]]
     [:div {:id "content"}
      body]
     [:div {:id "footer"}
      "Clojure Mini-Browser"]]))

(defn with-logging [handler]
  (fn [request]
    (let [start (System/nanoTime)
          response (handler request)
          elapsed (/ (double (- (System/nanoTime) start)) 1000000.0)]
      (when response
        (println (str (:uri request) " [" (:request-method request) "] " elapsed " msec"
                      "\n\tParameters " (:params request)
                      "\n\tSession " (:session request)))
        response))))

(defn read-param-string
  "Use Clojure reader to read a param strings, or return
   default is param string empty/nil."
  ([s] (read-param-string s nil))
  ([s default]
     (if (seq s)
       (read-string s)
       default)))

(defn normalize-options
  "Convert options from string form (as coming in from web)
   to data structures as needed."
  [options]
  (-> (keywordize-keys options)
      (update-in [:selectors] read-param-string)
      (update-in [:headers] read-param-string)
      (update-in [:start] read-param-string 0)))

(defroutes namespace-routes
  (GET "/vars" []
       (html
        (minib-layout
         "Namespaces"
          nil
         (namespace/browser)))))

(defroutes class-routes
  (GET "/classes/*"
       {:keys [params query-params]}
       (let [classname (get params "*")
             cls (Class/forName classname)]
         (html
          (minib-layout
           classname
           nil
           (class/render cls (normalize-options query-params) cls))))))

(defroutes var-routes
  (GET
   "/vars/*"
   {:keys [params query-params]}
   (let [qname (get params "*")
         [ns var] (str/split qname #"/")]
     (namespace/safe-load-ns ns)
     (html
      (minib-layout
       qname
       nil
       (if var
         (data/render (find-var (symbol qname)) (normalize-options query-params))
         (namespace/var-browser ns)))))))

(defn- redirect-to
  [a-var] 
  (let [ns (-> a-var meta :ns str) 
        v  (java.net.URLEncoder/encode (-> a-var meta :name str))]
    (redirect (str "/vars/" ns "/" v))))

(defroutes search-route
  (GET "/search" {{:strs [query mode]} :params}
    (let [vars (search/search query mode)] 
      (if (= (count vars) 1) 
        (redirect-to (first vars)) 
        (minib-layout "Search" query (search/search-layout query vars))))))

(def dynamic-routes (routes (-> namespace-routes examples/with-recent-history)
                            var-routes
                            search-route
                            class-routes))

(defroutes static-routes
  (resources/resources "/" {:root "/public"})
  (route/not-found "not found"))

(defroutes app
  (routes dynamic-routes
          #_(-> dynamic-routes with-logging)
          static-routes))

(defprotocol Inspector
  (launch [_])
  (inspect [_ obj options]))

(defrecord Instance [port]
  Inspector
  (launch [_]
          (run-jetty (var app) {:port port
                                :join? false}))
  (inspect [_ obj options]
           (let [query (breadcrumb/options->query-string options)]
             (if (class? obj)
               (browse-url (str "http://localhost:" port
                                "/classes/" (.getName obj) "?"
                                query))
               (browse-url (str "http://localhost:" port
                                (history/add obj) "&"
                                query))))))
