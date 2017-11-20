(ns feedparser-clj.core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st])
  (:import [com.sun.syndication.io SyndFeedInput XmlReader]
           [java.net URL]
           [java.io InputStreamReader]
           [com.sun.syndication.feed.synd SyndFeed]))

(defrecord feed [authors author categories contributors copyright description
                 encoding entries feed-type image language link entry-links
                 published-date title uri])

(defrecord entry [authors author categories contents contributors description
                  enclosures link published-date title updated-date url])

(defrecord enclosure  [length type uri])
(defrecord person     [email name uri])
(defrecord category   [name taxonomy-uri])
(defrecord content    [type value])
(defrecord image      [description link title url])
(defrecord entry-link [href hreflang length rel title type])

(s/def ::length number?)
(s/def ::href string?)
(s/def ::url string?)
(s/def ::uri string?)
(s/def ::email string?)
(s/def ::value string?)
(s/def ::hreflang string?)
(s/def ::rel string?)
(s/def ::title string?)
(s/def ::type string?)
(s/def ::name string?)
(s/def ::taxonomy-uri string?)
(s/def ::author string?)
(s/def ::copyright string?)
(s/def ::encoding (s/nilable string?))
(s/def ::feed-type string?)
(s/def ::link string?)

(s/def ::feed-description string?)
(s/def ::feed-uri (s/nilable string?))

(s/def ::enclosure (s/keys :req-un [::length ::type ::url]))
(s/def ::content (s/keys :req-un [::type ::value]))
(s/def ::entry-link (s/keys :req-un [::length ::href ::hreflang ::rel ::title ::type ::link]))
(s/def ::category (s/keys :req-un [::name ::taxonomy-uri]))
(s/def ::person (s/keys :req-un [::email ::name ::uri]))
(s/def ::image (s/nilable (s/keys :req-un [::description ::link ::title ::url])))

(s/def ::authors (s/coll-of ::author))
(s/def ::categories (s/coll-of ::category))
(s/def ::contents (s/coll-of ::content))
(s/def ::contributors (s/coll-of ::person))
(s/def ::enclosures (s/coll-of ::enclosure))

(s/def ::description (s/nilable ::content))

(s/def ::published-date (partial instance? java.util.Date))
(s/def ::updated-date (s/nilable (partial instance? java.util.Date)))

(s/def ::entry (s/keys :req-un [::authors ::categories ::contents ::contributors ::enclosures
                                ::description ::author ::link ::published-date ::title ::updated-date]))

(s/def ::entries (s/coll-of ::entry))
(s/def ::entry-links (s/coll-of ::entry-link))

(s/def ::feed (s/keys :req-un [::authors ::categories ::contributors ::entries ::entry-links
                               ::image ::author ::copyright ::feed-description ::encoding ::feed-type ::language ::link ::published-date ::title ::feed-uri]))

(s/fdef obj->enclosure :args (s/alt :e ::enclosure) :ret ::enclosure)
(defn- obj->enclosure
  "Create enclosure struct from SyndEnclosure"
  [e]
  (map->enclosure {:length (.getLength e)
                   :type   (.getType e)
                   :url    (.getUrl e)}))

(s/fdef obj->content :args (s/alt :e ::content) :ret ::content)
(defn- obj->content
  "Create content struct from SyndContent"
  [c]
  (map->content {:type  (.getType c)
                 :value (.getValue c)}))

(s/fdef obj->entry-link :args (s/alt :e ::entry-link) :ret ::entry-link)
(defn- obj->entry-link
  "Create entry-link struct from SyndLink"
  [l]
  (map->entry-link {:href     (.getHref l)
              :hreflang (.getHreflang l)
              :length   (.getLength l)
              :rel      (.getRel l)
              :title    (.getTitle l)
              :type     (.getType l)}))

(s/fdef obj->category :args (s/alt :e ::category) :ret ::category)
(defn- obj->category
  "Create category struct from SyndCategory"
  [c]
  (map->category {:name         (.getName c)
                  :taxonomy-uri (.getTaxonomyUri c)}))

(s/fdef obj->person :args (s/alt :e ::person) :ret ::person)
(defn- obj->person
  "Create a person struct from SyndPerson"
  [sp]
  (map->person {:email (.getEmail sp)
                :name  (.getName sp)
                :uri   (.getUri sp)}))

(s/fdef obj->image :args (s/alt :e ::image) :ret ::image)
(defn- obj->image
  "Create image struct from SyndImage"
  [i]
  (map->image {:description (.getDescription i)
               :link        (.getLink i)
               :title       (.getTitle i)
               :url         (.getUrl i)}))

(s/fdef obj->entry :args (s/alt :e ::entry) :ret ::entry)
(defn- obj->entry
  "Create feed entry struct from SyndEntry"
  [e]
  (map->entry {:authors        (map obj->person    (seq (.getAuthors e)))
               :categories     (map obj->category  (seq (.getCategories e)))
               :contents       (map obj->content   (seq (.getContents e)))
               :contributors   (map obj->person    (seq (.getContributors e)))
               :enclosures     (map obj->enclosure (seq (.getEnclosures e)))
               :description    (if-let [d (.getDescription e)] (obj->content d))
               :author         (.getAuthor e)
               :link           (.getLink e)
               :published-date (.getPublishedDate e)
               :title          (.getTitle e)
               :updated-date   (.getUpdatedDate e)
               :uri            (.getUri e)}))

(s/fdef obj->feed :args (s/alt :e ::feed) :ret ::feed)
(defn- obj->feed
  "Create a feed struct from a SyndFeed"
  [f]
  (map->feed  {:authors        (map obj->person     (seq (.getAuthors f)))
               :categories     (map obj->category   (seq (.getCategories f)))
               :contributors   (map obj->person     (seq (.getContributors f)))
               :entries        (map obj->entry      (seq (.getEntries f)))
               :entry-links    (map obj->entry-link (seq (.getLinks f)))
               :image          (if-let [i (.getImage f)] (obj->image i))
               :author           (.getAuthor f)
               :copyright        (.getCopyright f)
               :feed-description (.getDescription f)
               :encoding         (.getEncoding f)
               :feed-type        (.getFeedType f)
               :language         (.getLanguage f)
               :link             (.getLink f)
               :published-date   (.getPublishedDate f)
               :title            (.getTitle f)
               :feed-uri         (.getUri f)}))

(defn- parse-internal [xmlreader]
  (let [feedinput (new SyndFeedInput)
        syndfeed  (.build feedinput xmlreader)]
    (obj->feed syndfeed)))

(defn ->url [s]
  (if (string? s) (URL. s) s))

(defn parse-feed "Get and parse a feed from a URL"
  ([feedsource]
   (parse-internal (XmlReader. (->url feedsource))))
  ([feedsource content-type]
   (parse-internal (XmlReader. (->url feedsource) content-type)))
  ([feedsource content-type lenient]
   (parse-internal (XmlReader. (->url feedsource) content-type lenient)))
  ([feedsource content-type lenient default-encoding]
   (parse-internal (XmlReader. (->url feedsource) content-type lenient default-encoding))))

(defn instrument-all []
  (st/instrument [obj->enclosure
                  obj->content
                  obj->entry-link
                  obj->category
                  obj->person
                  obj->image
                  obj->entry
                  obj->feed]))
