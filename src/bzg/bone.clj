#!/usr/bin/env bb

;; bone.clj — Browse BARK reports interactively.
;;
;; Standalone CLI tool: reads JSON produced by bark export from a file,
;; a URL, stdin, or a URLs file listing multiple report sources.
;; Displays via fzf (with detail on selection) or plain text fallback.
;;
;; Configuration (~/.config/bone/config.edn):
;;   {:my-addresses ["you@example.com" "alias@example.com"]}
;;
;; Usage:
;;   bone.clj [options]
;;   bone.clj clear           Empty the cache
;;   bone.clj update          Fetch/update reports from all sources
;;   bone.clj report          Print a triage summary for maintainers
;;
;; Options:
;;   -f, --file FILE          Read reports from a JSON file
;;   -u, --url  URL           Fetch reports from a URL
;;   -U, --urls-file FILE     Fetch & merge reports from URLs listed in FILE
;;   -M, --my-addresses EMAILS Your email(s), comma-separated (overrides config)
;;   -p, --min-priority 1-3   Only show reports with priority >= N
;;   -s, --min-score 0-7     Only show reports with status score >= N
;;   -n, --source NAME        Filter by source name
;;   -S, --skip-columns COLS  Columns to hide, comma-separated
;;   -m, --mine               Show only reports involving your address(es)
;;   -c, --closed             Include closed reports
;;   -a, --add-source PATH    Add a reports.json source (URL or path)
;;   -r, --remove-source PATH Remove a reports.json source
;;   -l, --list-sources       List configured sources
;;   -h, --help               Show help
;;   -                        Read JSON from stdin
;;
;; By default, bone shows all open reports from configured sources.

(ns bzg.bone
  (:require [babashka.process :as process]
            [babashka.http-client :as http]
            [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [babashka.cli :as cli]))

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

(def config-path
  (str (System/getProperty "user.home") "/.config/bone/config.edn"))

(def cache-dir
  (str (System/getProperty "user.home") "/.config/bone/cache"))

(def patches-cache-dir
  (str cache-dir "/patches"))

(def events-cache-dir
  (str cache-dir "/events"))

(def texts-cache-dir
  (str cache-dir "/texts"))

(def reports-cache-dir
  (str cache-dir "/reports"))

(defn- load-config []
  (let [f (io/file config-path)]
    (if (.exists f)
      (try
        (edn/read-string (slurp f))
        (catch Exception _
          (throw (ex-info (str "Config file is ill-formed: " config-path) {}))))
      {})))

(defn- load-sources
  "Load :sources from config.edn — a vector of {:url ... :repo ...} maps."
  []
  (vec (:sources (load-config))))

(defn- save-sources! [sources]
  (let [config  (load-config)
        deduped (->> sources (reduce (fn [m s] (assoc m (:url s) s)) {}) vals vec)
        updated (assoc config :sources deduped)]
    (.mkdirs (.getParentFile (io/file config-path)))
    (spit config-path (pr-str updated))))

(defn- add-source! [url]
  (let [sources (load-sources)]
    (if (some #(= (:url %) url) sources)
      (println (str "Already registered: " url))
      (do (save-sources! (conj sources {:url url}))
          (println (str "Added: " url))))))

(defn- remove-source! [url]
  (let [sources (load-sources)]
    (if (some #(= (:url %) url) sources)
      (do (save-sources! (remove #(= (:url %) url) sources))
          (println (str "Removed: " url)))
      (println (str "Not found: " url)))))

(defn- list-sources! []
  (let [sources (load-sources)]
    (if (empty? sources)
      (println "No sources configured. Use --add-source URL_OR_PATH to add one.")
      (doseq [{:keys [url repo]} sources]
        (println (str "  " url (when repo (str "  (repo: " repo ")"))))))))

;; ---------------------------------------------------------------------------
;; Data loading
;; ---------------------------------------------------------------------------

(def min-bark-format "0.9.0")

(def ^:private related-kind-keys
  "Per-kind relation fields emitted by bark 0.9.0. Each holds a vector of
  `{:message-id ... :type ... :subject ... :archived-at ...}` entries."
  [:related-to :resolves :resolved-by
   :supersedes :superseded-by
   :duplicates :duplicated-by])

(defn- synthesize-related
  "Aggregate per-kind relation fields into a single :related vector,
  deduped by message-id, for the UI to read."
  [report]
  (let [merged (->> related-kind-keys
                    (mapcat report)
                    (filter :message-id)
                    (group-by :message-id)
                    vals
                    (mapv first))]
    (cond-> report
      (seq merged) (assoc :related merged))))

(defn- version-< [a b]
  (loop [as (str/split a #"\.")
         bs (str/split b #"\.")]
    (let [ai (parse-long (or (first as) "0"))
          bi (parse-long (or (first bs) "0"))]
      (cond
        (< ai bi) true
        (> ai bi) false
        (and (empty? (rest as)) (empty? (rest bs))) false
        :else (recur (rest as) (rest bs))))))

(defn- load-json-string [s]
  (json/parse-string s keyword))

(defn- url? [s]
  (boolean (re-find #"^https?://" s)))

(defn- strip-file-scheme
  "Strip file:// prefix if present."
  [s]
  (cond-> s (str/starts-with? s "file://") (str/replace-first "file://" "")))

(defn- source-base-dir
  "Compute the base directory from a source path/URL.
  For file paths, returns the parent directory.
  For HTTP URLs, returns nil (use base-url instead)."
  [src]
  (when-not (url? src)
    (when-let [parent (.getParent (io/file (strip-file-scheme src)))]
      (str parent "/"))))

(defn- source-base-url
  "Derive a base URL from an HTTP source URL.  BARK serves the JSON from a
  `reports/` subdirectory while attachments (patches/, events/, text/) are
  siblings of `reports/`.  So we strip the filename, then strip `reports/`
  when present.
  E.g. \"https://example.org/tracker/reports/all.json\" => \"https://example.org/tracker/\"
       \"https://example.org/data.json\"                => \"https://example.org/\"
  Returns nil for non-HTTP sources."
  [src]
  (when (url? src)
    (when-let [i (str/last-index-of src "/")]
      (let [dir (subs src 0 (inc i))]
        (if (str/ends-with? dir "/reports/")
          (subs dir 0 (- (count dir) (count "reports/")))
          dir)))))

(defn- fetch-source-body
  "Fetch raw JSON body string from a source (URL or file path)."
  [src]
  (if (url? src)
    (let [resp (http/get src {:headers {"Accept" "application/json"}})]
      (when (not= 200 (:status resp))
        (throw (ex-info (str "HTTP " (:status resp)) {:url src :status (:status resp)})))
      (:body resp))
    (slurp (strip-file-scheme src))))

(defn- unwrap-envelope
  "Unwrap a reports.json envelope. Returns {:reports [...]}.
  Injects :source and :base-url from envelope into each report."
  [data]
  (let [fv (:bark-format data)]
    (when (and fv (version-< fv min-bark-format))
      (binding [*out* *err*]
        (println (str "Warning: bark-format " fv
                      " is older than minimum supported version "
                      min-bark-format))))
    (let [reports  (or (:reports data) [])
          src-name (:source data)
          base-url (:base-url data)]
      {:reports (mapv (fn [r]
                        (-> r
                            (cond-> (and src-name (not (:source r))) (assoc :source src-name)
                                    base-url                         (assoc :base-url base-url))
                            synthesize-related))
                      reports)})))

(defn- inject-base-dir
  "Inject :base-dir into each report from a source path."
  [result src]
  (if-let [base (source-base-dir src)]
    (update result :reports (fn [rs] (mapv #(assoc % :base-dir base) rs)))
    result))

(defn- inject-base-url
  "Inject :base-url into each report from a source URL, when not already set."
  [result src]
  (if-let [base (source-base-url src)]
    (update result :reports (fn [rs] (mapv #(if (:base-url %) % (assoc % :base-url base)) rs)))
    result))

(defn- load-from-file [path]
  (when-not (.exists (io/file path))
    (throw (ex-info (str "File not found: " path) {:path path})))
  (inject-base-dir (unwrap-envelope (load-json-string (slurp path))) path))

(defn- load-from-url [url]
  (inject-base-url (unwrap-envelope (load-json-string (fetch-source-body url))) url))

(defn- load-from-stdin []
  (unwrap-envelope (load-json-string (slurp *in*))))

(defn- merge-results
  "Reduce a seq of {:reports [...]} into one."
  [results]
  {:reports (into [] (mapcat :reports) results)})

(defn- load-from-urls-file [path]
  (when-not (.exists (io/file path))
    (throw (ex-info (str "URLs file not found: " path) {:path path})))
  (let [urls (->> (str/split-lines (slurp path))
                  (map str/trim)
                  (remove #(or (str/blank? %) (str/starts-with? % "#"))))]
    (merge-results (map load-from-url urls))))

;; --- Cache ---

(defn- source->cache-file
  "Deterministic cache filename for a source URL/path."
  [src]
  (let [h (format "%08x" (hash src))
        safe (str/replace src #"[^a-zA-Z0-9._-]" "_")
        prefix (subs safe 0 (min 80 (count safe)))]
    (str reports-cache-dir "/" prefix "-" h ".json")))

(defn- cache-read
  "Read cached reports.json for a source. Returns nil if not cached."
  [src]
  (let [f (io/file (source->cache-file src))]
    (when (.exists f)
      (load-json-string (slurp f)))))

(defn- cache-write!
  "Write raw JSON string to cache for a source."
  [src body]
  (let [f (io/file (source->cache-file src))]
    (.mkdirs (.getParentFile f))
    (spit f body)))

(defn- clear-cache! []
  (when (fs/exists? cache-dir)
    (fs/delete-tree cache-dir))
  (println "Cache cleared."))

(defn- load-one-source-cached
  "Load reports from cache if available, otherwise fetch and cache."
  [src]
  (let [data (or (cache-read src)
                 (let [body (fetch-source-body src)]
                   (cache-write! src body)
                   (load-json-string body)))]
    (-> (unwrap-envelope data)
        (inject-base-dir src)
        (inject-base-url src))))

(defn- update-sources-cache!
  "Fetch all configured sources and update cache."
  []
  (let [sources (load-sources)]
    (when (empty? sources)
      (throw (ex-info "No sources configured." {})))
    (doseq [{:keys [url]} sources]
      (try
        (cache-write! url (fetch-source-body url))
        (println (str "  Updated: " url))
        (catch Exception e
          (binding [*out* *err*]
            (println (str "  [warn] Failed to update " url ": " (.getMessage e)))))))))

(defn- load-from-sources
  "Load and merge reports from all configured sources."
  []
  (let [sources (load-sources)]
    (when (empty? sources)
      (throw (ex-info (str "No sources configured and no -f/-u/-U/- given.\n"
                           "Add a source:  bone --add-source URL_OR_PATH\n"
                           "Or use:        bone -f FILE | -u URL | -")
                      {})))
    (merge-results
     (keep (fn [{:keys [url]}]
             (try
               (load-one-source-cached url)
               (catch Exception e
                 (binding [*out* *err*]
                   (println (str "  [warn] Failed to load " url ": " (.getMessage e))))
                 nil)))
           sources))))

;; ---------------------------------------------------------------------------
;; Local state (~/.config/bone/state.org)
;;
;; Per-report `flag` (nil, :todo, :sticky, :done) and `read-at` (set when the
;; user has visited the item). Persisted as Org so it can be opened in Emacs,
;; hand-edited, or grepped. Keyed by RFC-2822 message-id, which is stable
;; across re-fetches and globally unique.
;; ---------------------------------------------------------------------------

(def state-edn-path
  (str (System/getProperty "user.home") "/.config/bone/state.edn"))

(def todo-org-path
  "Where `bone todo` writes the exported Org file."
  (str (System/getProperty "user.home") "/.config/bone/todo.org"))

(def ^:private org-header
  "#+TITLE: bone state\n#+TODO: TODO STICKY | DONE\n\n")

(def ^:private flag->keyword
  {:todo "TODO" :sticky "STICKY" :done "DONE"})

(def ^:private org-ts-fmt
  (java.time.format.DateTimeFormatter/ofPattern
   "yyyy-MM-dd EEE HH:mm" java.util.Locale/ENGLISH))

(defn- iso-now [] (str (java.time.Instant/now)))

(defn- format-org-timestamp
  "Format an ISO-8601 or 'YYYY-MM-DD HH:MM' string as an Org inactive
  timestamp '[YYYY-MM-DD Day HH:MM]'. Falls back to wrapping the raw value."
  [s]
  (if (or (nil? s) (str/blank? s)) ""
      (or (try (let [inst (java.time.Instant/parse s)
                     zdt  (.atZone inst (java.time.ZoneId/systemDefault))]
                 (str "[" (.format zdt org-ts-fmt) "]"))
               (catch Exception _ nil))
          (try (let [ldt (java.time.LocalDateTime/parse
                          s (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))]
                 (str "[" (.format ldt org-ts-fmt) "]"))
               (catch Exception _ nil))
          (str "[" s "]"))))

(defn- load-state []
  (let [f (io/file state-edn-path)]
    (if (.exists f)
      (try (edn/read-string (slurp f))
           (catch Exception e
             (binding [*out* *err*]
               (println (str "Warning: cannot parse " state-edn-path ": "
                             (.getMessage e) " — using empty state.")))
             {}))
      {})))

(defn- save-state! [state]
  (.mkdirs (.getParentFile (io/file state-edn-path)))
  ;; Newline per entry for hand-edit friendliness without depending on pprint.
  (spit state-edn-path
        (str "{"
             (str/join "\n " (map (fn [[k v]] (str (pr-str k) " " (pr-str v))) state))
             "}\n")))

(defn- type->tag
  "Convert a report type ('patch', 'bug', ...) to ':patch:'. Returns nil for
  empty/unsuitable values."
  [t]
  (when (and t (re-matches #"[a-zA-Z][\w-]*" (str t)))
    (str ":" (str/lower-case t) ":")))

(defn- render-entry [mid {:keys [flag read-at author subject type created]}]
  (let [kw (flag->keyword flag)
        tag (type->tag type)]
    (str "* " (when kw (str kw " ")) (or subject "(no subject)")
         (when tag (str "    " tag)) "\n"
         "  :PROPERTIES:\n"
         "  :MESSAGE-ID: " mid "\n"
         (when created
           (str "  :CREATED:    " (format-org-timestamp created) "\n"))
         (when author
           (str "  :AUTHOR:     " author "\n"))
         (when read-at
           (str "  :READ-AT:    " (format-org-timestamp read-at) "\n"))
         "  :END:\n")))

(defn- render-org-state [state]
  (let [entries (sort-by (fn [[_ v]] (or (:created v) "")) #(compare %2 %1) state)]
    (str org-header
         (str/join "\n" (map (fn [[mid v]] (render-entry mid v)) entries)))))

(defn- write-todo-org!
  "Generate todo.org from the current state.edn, restricted to entries flagged
  :todo. :sticky and :read entries are skipped. Returns [path n-todos]."
  [out-path]
  (let [todos (into {} (filter (fn [[_ v]] (= :todo (:flag v))) (load-state)))]
    (.mkdirs (.getParentFile (io/file out-path)))
    (spit out-path (render-org-state todos))
    [out-path (count todos)]))

(defn- author-string [report]
  (let [n (:from-name report) e (:from report)]
    (cond (and n e (not (str/blank? n))) (str n " <" e ">")
          e e
          n n
          :else nil)))

(defn- enrich-entry
  "Build/refresh metadata for a state entry from a report. Only fields that
  are non-nil in `report` are written, so hand-edited subjects/authors are
  preserved when the source report is missing those fields."
  [existing report]
  (cond-> existing
    (:subject report)      (assoc :subject (:subject report))
    (:type report)         (assoc :type    (:type report))
    (author-string report) (assoc :author  (author-string report))
    (:date report)         (assoc :created (:date report))))

(defn- apply-transition
  "Apply an action ∈ #{:read :todo :sticky} to the state. All three actions
  toggle off when the corresponding state is already set. Entries that end up
  with neither :flag nor :read-at are dissoc'd."
  [state action mid report]
  (if (nil? mid)
    state
    (let [base      (enrich-entry (or (get state mid) {}) report)
          new-entry (case action
                      :read   (if (:read-at base)
                                (dissoc base :read-at)
                                (assoc base :read-at (iso-now)))
                      :todo   (if (= (:flag base) :todo)
                                (dissoc base :flag)
                                (assoc base :flag :todo))
                      :sticky (if (= (:flag base) :sticky)
                                (dissoc base :flag)
                                (assoc base :flag :sticky)))]
      (if (and (nil? (:flag new-entry)) (nil? (:read-at new-entry)))
        (dissoc state mid)
        (assoc state mid new-entry)))))

(defn- visible-by-state?
  "Show a report given the local state and the active view mode.
   :default and :all → everything (the :read items are dropped upstream by
   `startup-filter` for :default; once the session is loaded, marking an
   item :read keeps it visible until the next reload).
   :todo   → only :todo.
   :sticky → :todo and :sticky."
  [state report view]
  (let [flag (:flag (get state (:message-id report)))]
    (case view
      :todo   (= flag :todo)
      :sticky (#{:todo :sticky} flag)
      true)))

(defn- mark-prefix
  "Single-character prefix for the leftmost column.
  Flag wins over read-at: a :todo+read item shows '!', not 'r'."
  [state mid]
  (let [s (get state mid)]
    (case (:flag s)
      :todo   "!"
      :sticky "*"
      (if (:read-at s) "r" " "))))

(defn- startup-filter
  "Apply the :default-mode startup filter: drop reports the user has marked
  :read with no active flag. Other view modes pass through unchanged."
  [reports user-state view-mode]
  (if (= view-mode :default)
    (remove (fn [r]
              (let [s (get user-state (:message-id r))]
                (and (:read-at s) (nil? (:flag s)))))
            reports)
    reports))

;; --- Session helpers (used by --internal-* subcommands) ---

(declare display-type)
(declare report->row)
(declare column-headers)
(declare normalize-skip-columns)
(declare tabulate)

(defn- write-session!
  "Serialize a session as JSON. Keywords/sets are converted to strings/vectors;
  read-session reverses these. JSON parses ~5x faster than EDN in babashka,
  which matters because every fzf reload re-reads the session."
  [path session]
  (-> session
      (update :view-mode   #(some-> % name))
      (update :skip-columns #(when % (vec %)))
      (update :types       #(when % (vec %)))
      (update :sources     #(when % (vec %)))
      (update :topics      #(when % (vec %)))
      json/generate-string
      (->> (spit path))))

(defn- read-session [path]
  (let [data (json/parse-string (slurp path) keyword)]
    (-> data
        (update :view-mode    #(some-> % keyword))
        (update :skip-columns #(when % (set %)))
        (update :types        #(when % (set %)))
        (update :sources      #(when % (set %)))
        (update :topics       #(when % (set %))))))

(defn- session-visible
  "Apply types/sources/topics filters and state visibility to the session's
  reports. Returns the ordered visible reports."
  [{:keys [reports types sources topics all-types all-sources all-topics view-mode]}
   user-state]
  (let [active-types   (or types (set all-types))
        active-sources (or sources (set all-sources))
        active-topics  (or topics (set all-topics))
        view           (or view-mode :default)]
    (->> reports
         (filter #(contains? active-types (display-type (:type %))))
         (filter #(or (empty? all-sources)
                      (contains? active-sources (:source %))))
         (filter #(or (empty? all-topics)
                      (and (nil? topics) (nil? (:topic %)))
                      (contains? active-topics (:topic %))))
         (filter #(visible-by-state? user-state % view))
         vec)))

(defn- session-render
  "Render the current visible list to aligned text rows. Returns the vector
  produced by tabulate: [header-line row0 row1 ...]."
  [session user-state]
  (let [{:keys [show-type? show-src? skip-columns]} session
        skip    (normalize-skip-columns skip-columns)
        visible (session-visible session user-state)
        header  (column-headers show-type? show-src? skip)
        rows    (mapv #(report->row % show-type? show-src? skip user-state) visible)]
    (tabulate (cons header rows))))

(defn- do-internal-print!
  "Internal entry point used by fzf reload bindings. Reads the session,
  optionally applies a mark transition for the report at visible-line N,
  and emits the (re-filtered) aligned list to stdout for fzf to consume.
  Args: SESSION-PATH [N ACTION]."
  [session-path n-str action-str]
  (let [session    (read-session session-path)
        pre-state  (load-state)
        post-state (or (when (and n-str action-str)
                         (when-let [n (try (Long/parseLong n-str) (catch Exception _ nil))]
                           (when-let [target (nth (session-visible session pre-state) n nil)]
                             (let [s (apply-transition pre-state
                                                       (keyword action-str)
                                                       (:message-id target)
                                                       target)]
                               (save-state! s)
                               s))))
                       pre-state)]
    (println (str/join "\n" (session-render session post-state)))))

(declare pick-sort!)
(declare pick-multi!)
(declare sort-reports)

(defn- do-internal-pick-sort! [session-path]
  (let [session (read-session session-path)]
    (when-let [idx (pick-sort!)]
      (write-session! session-path
                      (assoc session
                             :reports  (sort-reports (:reports session) idx)
                             :sort-idx idx)))))

(defn- do-internal-pick-multi! [session-path which]
  (let [session (read-session session-path)
        all (case which
              :types   (:all-types session)
              :sources (:all-sources session)
              :topics  (:all-topics session))]
    (when (seq all)
      (when-let [picked (pick-multi! (str (name which) "> ") all)]
        (write-session! session-path (assoc session which picked))))))

(defn- do-internal-clear-filters! [session-path]
  (let [session (read-session session-path)]
    (write-session! session-path
                    (assoc session :types nil :sources nil :topics nil))))

;; ---------------------------------------------------------------------------
;; Filtering
;; ---------------------------------------------------------------------------

(defn- normalize-addresses
  "Coerce :my-addresses (a string or vector of strings) into a set of
  lower-cased addresses."
  [v]
  (when v
    (let [xs (if (string? v) [v] v)]
      (into #{} (map str/lower-case) xs))))

(defn- involves-email?
  "True when the report involves any of the given email addresses."
  [report addresses]
  (let [addrs (if (set? addresses) addresses (normalize-addresses addresses))]
    (some #(when % (contains? addrs (str/lower-case %)))
          [(:from report)
           (:acked report) (:owned report) (:closed report)
           (:acked-proxy report) (:owned-proxy report) (:closed-proxy report)])))


;; ---------------------------------------------------------------------------
;; Formatting helpers
;; ---------------------------------------------------------------------------

(defn- multiple-sources? [reports]
  (> (count (distinct (keep :source reports))) 1))

(defn- date-only
  "Extract just the date portion, stripping any leading weekday and trailing time.
  Handles both 'Sat Mar 07 14:30' and '2026-03-07 14:30' style dates."
  [s]
  (if (and s (seq s))
    (let [s (str/trim s)
          ;; Strip leading 'Mon ' / 'Tue ' etc.
          s (str/replace-first s #"^[A-Z][a-z]{2}\s+" "")
          ;; Strip time portion after date (either 'HH:MM...' or 'THH:MM...')
          s (str/replace-first s #"[T ]?\d{2}:\d{2}.*" "")]
      (str/trim s))
    ""))

(defn- parse-votes
  "Parse a votes string like \"3/5\" (sum/total) into [sum total].
  Returns [0 0] on nil."
  [v]
  (if (and v (re-matches #"-?\d+/\d+" v))
    (mapv parse-long (str/split v #"/"))
    [0 0]))

(defn- vote-cookie
  "Format a vote cookie like \"[3/5] \" from a report's :votes field.
  Returns empty string when no votes."
  [report]
  (if-let [v (:votes report)]
    (str "[" v "] ")
    ""))

(defn- days-until
  "Days from now to the report's date field (yyyy-mm-dd).
  Negative = past. Returns nil when field is absent."
  [report field]
  (when-let [d (get report field)]
    (try
      (.between java.time.temporal.ChronoUnit/DAYS
                (java.time.LocalDate/now)
                (java.time.LocalDate/parse d))
      (catch Exception _ nil))))

(defn- deadline-col
  "Format the deadline column: days as string, or empty."
  [report]
  (if-let [d (days-until report :deadline)] (str d) ""))

(defn- display-type
  "Map a BARK report type to its short display form."
  [t]
  (case t "announcement" "announce" t))

(defn- truncate
  "Truncate string s to at most n characters."
  [s n]
  (if (> (count s) n) (subs s 0 n) s))

(defn- report-flags+score
  "Compute flags string and numeric score from report fields.
  Flags: A=acked O=owned, third char: C=canceled R=resolved E=expired -=open.
  Score matches bark-index.clj: acked=1, owned=2, open=4 (closed=0)."
  [report]
  (let [a? (:acked report)
        o? (:owned report)
        c? (:closed report)
        cr (:close-reason report)]
    {:flags (str (if a? "A" "-")
                 (if o? "O" "-")
                 (case cr
                   "canceled" "C"
                   "resolved" "R"
                   "expired"  "E"
                   (if c? "R" "-")))
     :score (+ (if a? 1 0) (if o? 2 0) (if c? 0 4))}))

(def ^:private column-aliases
  "Short aliases for column names."
  {"p" "priority", "d" "deadline", "#" "replies", "!" "mark"})

(defn- normalize-column [col]
  (get column-aliases col col))

(defn- normalize-skip-columns [skip]
  (set (map normalize-column (or skip #{}))))

(defn- report-columns
  "Return a vector of column values for a report.
  skip is a set of column names to hide (e.g. #{\"owner\" \"att\"}).
  user-state is the loaded state.org map; nil means no mark column."
  [report show-type? show-src? skip user-state]
  (let [skip (normalize-skip-columns skip)]
    (concat
     (when-not (skip "mark")                     [(mark-prefix user-state (:message-id report))])
     (when (and show-type? (not (skip "type")))   [(display-type (:type report ""))])
     (when (and show-src?  (not (skip "source"))) [(truncate (:source report "") 10)])
     (when-not (skip "priority") [(case (:priority report 0) 3 "A" 2 "B" 1 "C" " ")])
     (when-not (skip "deadline") [(deadline-col report)])
     (when-not (skip "flags")    [(:flags (report-flags+score report))])
     (when-not (skip "replies")  [(str (:replies report 0))])
     (when-not (skip "author")   [(truncate (:from report "?") 15)])
     (when-not (skip "owner")    [(truncate (:owned report "") 15)])
     (when-not (skip "date")     [(date-only (:date report))])
     (when-not (skip "att")      [(str (when (seq (:patches report)) "+")
                                       (when (seq (:events report))  "@")
                                       (when (seq (:texts report))   "#")
                                       (when (:awaiting report)      "?")
                                       (when (seq (:related report)) "~"))])
     [(str (vote-cookie report) (:subject report "(no subject)"))])))

(defn- report->row
  "Format a report as a tab-separated row for fzf display."
  [report show-type? show-src? skip user-state]
  (str/join "\t" (report-columns report show-type? show-src? skip user-state)))

(defn- extra-str [report]
  (->> [(:source report)
        (:version report)
        (:topic report)
        (:patch-seq report)
        (when-let [sources (seq (:patch-source report))]
          (str "src:" (str/join "," sources)))
        (when-let [ps (seq (:patches report))]
          (str "📎" (count ps)))
        (when-let [s (:series report)]
          (str "series:" (:received s) "/" (:expected s)
               (when (:closed s) " closed")))
        (when-let [related (seq (:related report))]
          (str "→" (str/join "," (distinct (map (comp display-type :type) related)))))]
       (keep identity)
       seq
       (str/join " ")))

(defn- report->line
  "Format a report as a plain text line."
  [report show-type? show-src? skip user-state]
  (let [line (str/join "  " (report-columns report show-type? show-src? skip user-state))]
    (if-let [e (extra-str report)]
      (str line " " e)
      line)))

;; ---------------------------------------------------------------------------
;; Display
;; ---------------------------------------------------------------------------

(defn- fzf-available? []
  (try
    (zero? (:exit (process/shell {:out :string :err :string :continue true} "fzf" "--version")))
    (catch Exception _ false)))

(defn- tmp-path
  "Build /tmp/bone-<tag>-<ts><ext> for a temp file. Pass a single shared
  timestamp when several related files must be cleaned up together."
  [tag ts ext]
  (str (System/getProperty "java.io.tmpdir") "/bone-" tag "-" ts ext))

(defn- tabulate
  "Align tab-separated rows into fixed-width columns."
  [rows]
  (-> (process/shell {:in (str/join "\n" rows) :out :string}
                     "column" "-t" "-s" "\t")
      :out
      str/trim
      str/split-lines))

(defn- command-available? [cmd]
  (try
    (zero? (:exit (process/shell {:out :string :err :string :continue true} "which" cmd)))
    (catch Exception _ false)))

(def ^:private text-browser-cmds
  [["w3m"   ["w3m" "-o" "confirm_qq=false"]]
   ["lynx"  ["lynx"]]
   ["links" ["links"]]])

(defn- platform-opener []
  (let [os (str/lower-case (System/getProperty "os.name"))]
    (cond
      (str/includes? os "mac") ["open"]
      (str/includes? os "win") ["start"]
      :else                    ["xdg-open"])))

(defn- browse-cmd
  "Return the best available command for viewing a URL.
  Honors :text-browser from config, then probes w3m/lynx/links,
  falls back to platform opener."
  [config]
  (or (when-let [b (:text-browser config)]
        (some (fn [[cmd v]] (when (= cmd b) v)) text-browser-cmds))
      (some (fn [[cmd v]] (when (command-available? cmd) v))
            text-browser-cmds)
      (platform-opener)))

(defn- attachment-paths
  "Return a vector of {:url ... :cache-path ...} for attachments of a given kind.
  `kind` is the report key (:events or :texts), `subdir` the URL/cache subdirectory."
  [report kind subdir cache-dir]
  (when-let [atts (seq (get report kind))]
    (let [base       (or (:base-dir report)
                         (when-let [bp (:base-url report)]
                           (if (str/ends-with? bp "/") bp (str bp "/"))))
          src-name   (or (:source report) "default")]
      (mapv (fn [a]
              (let [file       (:file a)
                    cache-file (str/replace-first file #"^(\.\./?)+" "")]
                {:url        (if base (str base subdir "/" file) file)
                 :cache-path (str cache-dir "/" src-name "/" cache-file)}))
            atts))))

(defn- patch-paths
  "Return a vector of {:url ... :cache-path ... :filename ...} for each patch in a report.
  Delegates to attachment-paths, then adds :filename from patch metadata."
  [report]
  (when-let [paths (attachment-paths report :patches "patches" patches-cache-dir)]
    (mapv (fn [path patch]
            (assoc path :filename (:patch/filename patch (:file patch))))
          paths (:patches report))))

(defn- event-paths [report] (attachment-paths report :events "events" events-cache-dir))
(defn- text-paths  [report] (attachment-paths report :texts  "text"  texts-cache-dir))

(def ^:private diff-pager-cmds
  [["delta"          ["delta" "--paging" "always"]]
   ["bat"            ["bat" "--style=plain" "--language=diff" "--paging=always"]]
   ["diff-so-fancy"  ["diff-so-fancy"]]])

(def ^:private stdin-diff-pagers
  "Pagers that read from stdin rather than a file argument."
  #{"delta" "diff-so-fancy"})

(defn- patch-pager
  "Return a command vector for viewing patches with syntax highlighting.
  Honors :diff-pager from config, then probes delta/bat, falls back to $PAGER/less."
  [config]
  (or (when-let [p (:diff-pager config)]
        (some (fn [[cmd v]] (when (= cmd p) v)) diff-pager-cmds))
      (some (fn [[cmd v]] (when (and (not= cmd "diff-so-fancy") (command-available? cmd)) v))
            diff-pager-cmds)
      [(or (System/getenv "PAGER") "less")]))

;; ---------------------------------------------------------------------------
;; Sorting
;; ---------------------------------------------------------------------------

(defn- parse-date-ms
  "Parse a date-raw string to epoch millis for sorting. Returns 0 on failure."
  [s]
  (if (or (nil? s) (str/blank? s))
    0
    (or (try (.toEpochMilli (java.time.Instant/parse s))
             (catch Exception _ nil))
        (try (-> (java.time.LocalDate/parse s)
                 (.atStartOfDay java.time.ZoneOffset/UTC)
                 .toInstant
                 .toEpochMilli)
             (catch Exception _ nil))
        0)))

(def sort-options
  ;; Each entry: [label key-fn cmp ?needs-state]. key-fn takes [report state],
  ;; but only the entry tagged needs-state pays the cost of loading state.org.
  [["date (newest)"    (fn [r _] (- (parse-date-ms (:date-raw r))))                              compare]
   ["date (oldest)"    (fn [r _] (parse-date-ms (:date-raw r)))                                  compare]
   ["priority (high)"  (fn [r _] (- (:priority r 0)))                                            compare]
   ["status (active)"  (fn [r _] (- (:score (report-flags+score r))))                            compare]
   ["deadline (closest)"  (fn [r _] (or (days-until r :deadline) Integer/MAX_VALUE))             compare]
   ["deadline (farthest)" (fn [r _] (- (or (days-until r :deadline) Integer/MIN_VALUE)))         compare]
   ["replies (most)"   (fn [r _] (- (:replies r 0)))                                             compare]
   ["votes"            (fn [r _] (let [[sum total] (parse-votes (:votes r))]
                                   (if (pos? total) (- (/ (double sum) total)) 0.0)))            compare]
   ["activity (recent)" (fn [r _] (- (parse-date-ms (:last-activity r))))                        compare]
   ["activity (oldest)" (fn [r _] (parse-date-ms (:last-activity r)))                            compare]
   ["awaiting (recent)" (fn [r _] (if (:awaiting r) (- (parse-date-ms (:date-raw r))) Long/MAX_VALUE)) compare]
   ["awaiting (oldest)" (fn [r _] (if (:awaiting r) (parse-date-ms (:date-raw r)) Long/MAX_VALUE))    compare]
   ["type"              (fn [r _] (display-type (:type r "")))                                   compare]
   ["marked (todo, sticky first)"
    (fn [r state]
      (let [flag (:flag (get state (:message-id r)))
            group (case flag :todo 0 :sticky 1 2)]
        [group (- (parse-date-ms (:date-raw r)))]))
    compare :needs-state]])

(def ^:private tmux? (some? (System/getenv "TMUX")))

(def ^:private exec-action
  ;; In tmux, use execute-silent so the main list stays drawn while the
  ;; dispatched action shows in a popup. Outside tmux, fall back to execute
  ;; (which releases fzf's screen for fullscreen less, etc.).
  (if tmux? "execute-silent" "execute"))

(def ^:private picker-window-args
  ;; Inside tmux 3.3+, fzf can open in a popup overlaying the current pane,
  ;; so the main list stays visible behind. Outside tmux, fall back to an
  ;; inline 40% bottom strip.
  (if tmux?
    ["--tmux" "bottom,40%"]
    ["--height" "40%"]))

(defn- pick-sort!
  "Let user pick a sort order via fzf. Returns index or nil."
  []
  (let [labels (mapv first sort-options)
        input  (str/join "\n" labels)
        {:keys [exit out]}
        (apply process/shell {:in input :out :string :continue true}
               (concat ["fzf" "--prompt" "sort by> " "--no-sort" "--reverse"]
                       picker-window-args))]
    (when (zero? exit)
      (let [selected (str/trim out)
            idx (.indexOf ^java.util.List labels selected)]
        (when (>= idx 0) idx)))))

(defn- sort-reports [reports sort-idx]
  (let [[_ key-fn cmp needs] (nth sort-options sort-idx)
        user-state           (when (= needs :needs-state) (load-state))]
    (sort-by #(key-fn % user-state) cmp reports)))

(defn- pick-multi!
  "Let user pick from `all` via fzf multi-select.
  '[all]' resets to full set. Returns a set, or nil on cancel."
  [prompt all]
  (let [input (str/join "\n" (cons "[all]" all))
        {:keys [exit out]}
        (apply process/shell {:in input :out :string :continue true}
               (concat ["fzf" "--multi" "--prompt" prompt
                        "--no-sort" "--reverse"]
                       picker-window-args))]
    (when (zero? exit)
      (let [selected (set (remove str/blank? (str/split-lines (str/trim out))))]
        (if (contains? selected "[all]")
          (set all)
          selected)))))

;; ---------------------------------------------------------------------------
;; Dispatch script for fzf execute bindings
;; ---------------------------------------------------------------------------

(defn- shell-escape
  "Escape a string for use inside single quotes in shell."
  [s]
  (str "'" (str/replace s "'" "'\\''") "'"))

(def ^:private usage-text
  (str/join
   "\n"
   ["Keys:"
    "  Ctrl-n / Ctrl-p            Move to next / previous line"
    "  Enter                      View report in terminal browser"
    "  Ctrl-o                     Open report in system browser"
    "  Ctrl-v                     View attachment (patch, ics, txt)"
    "  Ctrl-/                     Show related reports"
    "  Ctrl-s                     Change sort order"
    "  Ctrl-b / Ctrl-r / Ctrl-t   Filter by source / type / topic"
    "  Ctrl-x                     Remove all filters"
    "  Ctrl-u                     Update cache and reload"
    "  Alt-!                      Toggle :todo (column shows '!')"
    "  Alt-*                      Toggle :sticky (column shows '*')"
    "  Alt-Enter                  Toggle :read (column shows 'r'; hidden at next launch unless --all)"
    "  Ctrl-h                     Show this help"
    ""
    "Columns:"
    "  !       Local mark: '!' = :todo, '*' = :sticky"
    "  P       Priority: A = high, B = medium, C = low"
    "  D       Days until deadline (negative = past)"
    "  Flags   (A)cked (O)wned (C)anceled/(R)esolved/(E)xpired"
    "  #       Number of replies"
    "  Att     '+' patches, '@' events, '#' texts, '?' awaiting reply, '~' related"]))

(defn- page-cmd-str
  "Return a shell command string to page a file, given pager config.
  Inside a tmux popup we:
   - drop -F (so a short patch doesn't auto-quit and flash the popup closed),
   - drop -X (so less uses the alt-screen and the content sits at the top),
   - append '+g' to scroll to line 1 at startup. Pipe-mode less can otherwise
     leave the cursor at the end of the streamed content, and any user-set
     LESS=+G would also park us at the bottom.
  delta and bat run their own internal less; we use their --pager option to
  force our exact invocation so flags actually take effect."
  [pager stdin? dsf? file-var]
  (let [pname      (first pager)
        less-cmd   (if tmux? "less -R +g" "less -RFX")
        ;; Force delta/bat to spawn OUR less invocation; otherwise their
        ;; hardcoded -RXF (and any user $LESS) wins and -R may be lost.
        pager-vec  (if (and tmux? (#{"delta" "bat"} pname))
                     (concat pager ["--pager" less-cmd])
                     pager)]
    (if stdin?
      (if dsf?
        (str "cat " file-var " | diff-so-fancy | " less-cmd)
        (str (str/join " " (map shell-escape pager-vec)) " < " file-var))
      (str (str/join " " (map shell-escape pager-vec)) " " file-var))))

(defn- write-dispatch-script!
  "Write a temp shell script that maps fzf line numbers to actions.
  The script takes two args: ACTION (open|view|browse|help) and LINE_NUMBER.
  Attachments are fetched lazily on first view.
  help-path is a file containing the keymap text shown by Ctrl-h.
  When invoked inside tmux, actions that produce visible output (`help` and
  any `view:N` for a report that has attachments) re-exec the script inside
  a `tmux display-popup`, so the main list stays visible behind. Branches
  that would be silent (e.g. C-v on a report with no attachments) bypass the
  popup entirely — no flash."
  [dispatch-path help-path config visible]
  (let [browse  (browse-cmd config)
        pager   (patch-pager config)
        stdin?  (stdin-diff-pagers (first pager))
        dsf?    (= "diff-so-fancy" (first pager))
        plain-pager (or (System/getenv "PAGER") "less")
        sb      (StringBuilder.)
        hf      (shell-escape help-path)
        self    (shell-escape dispatch-path)
        ;; help fits this many lines (text + a couple for the less prompt).
        help-rows (+ 4 (count (str/split-lines usage-text)))
        ;; Per-N indices for actions that produce output worth popping.
        view-ns (->> visible
                     (map-indexed vector)
                     (keep (fn [[n r]]
                             (when (or (seq (patch-paths r))
                                       (seq (event-paths r))
                                       (seq (text-paths r)))
                               n))))
        open-ns (->> visible
                     (map-indexed vector)
                     (keep (fn [[n r]] (when (:archived-at r) n))))]
    (.append sb "#!/bin/sh\nACTION=\"$1\"\nN=\"$2\"\n")
    ;; Tmux-popup wrapper. Only wraps actions that will actually produce
    ;; output, so a no-op (e.g. C-v on a report with no attachments) doesn't
    ;; flash an empty popup. Help uses an exact-fit height; view/open use a
    ;; generous one (browsers often need room).
    (.append sb (str "if [ -n \"$TMUX\" ] && [ -z \"$BONE_IN_POPUP\" ]; then\n"
                     "  case \"$ACTION:$N\" in\n"
                     "    help:*)\n"
                     "      exec tmux display-popup -E -h " help-rows " -w 90% -y S \\\n"
                     "        \"BONE_IN_POPUP=1 sh " self " help\" ;;\n"
                     (when (seq view-ns)
                       (str "    " (str/join "|" (map #(str "view:" %) view-ns)) ")\n"
                            "      exec tmux display-popup -E -h 90% -w 90% -y S \\\n"
                            "        \"BONE_IN_POPUP=1 sh " self " view '$N'\" ;;\n"))
                     (when (seq open-ns)
                       (str "    " (str/join "|" (map #(str "open:" %) open-ns)) ")\n"
                            "      exec tmux display-popup -E -h 90% -w 90% -y S \\\n"
                            "        \"BONE_IN_POPUP=1 sh " self " open '$N'\" ;;\n"))
                     "  esac\n"
                     "fi\n"))
    ;; Lazy fetch helper: bone_fetch URL CACHE_PATH
    (.append sb (str/join "\n"
                          ["bone_fetch() {"
                           "  local url=\"$1\" dest=\"$2\""
                           "  if [ ! -f \"$dest\" ]; then"
                           "    mkdir -p \"$(dirname \"$dest\")\""
                           "    case \"$url\" in"
                           "      http://*|https://*) curl -sfLo \"$dest\" \"$url\" || wget -qO \"$dest\" \"$url\" ;;"
                           "      *) cp \"$url\" \"$dest\" ;;"
                           "    esac"
                           "  fi"
                           "}"
                           ""]))
    (.append sb (str "if [ \"$ACTION\" = \"help\" ]; then\n"
                     "  less -R " hf "\n"
                     "  exit 0\n"
                     "fi\n"))
    (.append sb "case \"$ACTION:$N\" in\n")
    (doseq [[n report] (map-indexed vector visible)]
      (let [url  (:archived-at report)
            ps   (patch-paths report)
            es   (event-paths report)
            ts   (text-paths report)]
        ;; open action (text browser / enter)
        (when url
          (.append sb (str "  open:" n ")\n"))
          (.append sb (str "    " (str/join " " (map shell-escape (conj browse url))) "\n"))
          (.append sb "    ;;\n"))
        ;; browse action (system browser / ctrl-o)
        (when url
          (.append sb (str "  browse:" n ")\n"))
          (let [opener (platform-opener)]
            (.append sb (str "    " (str/join " " (map shell-escape (conj opener url))) "\n")))
          (.append sb "    ;;\n"))
        ;; view action — view any attachment (patch, ics, txt)
        (let [groups (cond-> []
                       (seq ps) (conj {:label "patch" :items ps :diff? true})
                       (seq es) (conj {:label "event" :items es :diff? false})
                       (seq ts) (conj {:label " text" :items ts :diff? false}))]
          (when (seq groups)
            (.append sb (str "  view:" n ")\n"))
            (let [plain-page
                  ;; LESS env covers the case where $PAGER is something other
                  ;; than less; explicit args win when it IS less.
                  (fn [path] (str (when tmux? "LESS='-R +g' ")
                                  (shell-escape plain-pager) " " path))
                  emit-fetch-and-page
                  (fn [{:keys [url cache-path]} diff?]
                    (str "    bone_fetch " (shell-escape url) " " (shell-escape cache-path) "\n"
                         "    " (if diff?
                                  (page-cmd-str pager stdin? dsf? (shell-escape cache-path))
                                  (plain-page (shell-escape cache-path)))
                         "\n"))]
              (if (and (= 1 (count groups)) (= 1 (count (:items (first groups)))))
                ;; Single attachment — no picker needed
                (.append sb (emit-fetch-and-page (first (:items (first groups))) (:diff? (first groups))))
                ;; Flatten all attachments into one picker with "label: filename" entries
                (let [entries (for [{:keys [label items diff?]} groups
                                    item items]
                                {:label (str label ": " (last (str/split (:url item) #"/")))
                                 :item item
                                 :diff? diff?})
                      entry-labels (mapv :label entries)]
                  (.append sb (str "    PICK=$(printf "
                                   (shell-escape (str/join "\\n" entry-labels))
                                   " | fzf --prompt 'view> ' --no-sort --reverse)\n"))
                  (.append sb "    [ -z \"$PICK\" ] && exit 0\n")
                  (.append sb "    case \"$PICK\" in\n")
                  (doseq [{:keys [label item diff?]} entries]
                    (.append sb (str "      " (shell-escape label) ")\n"))
                    (.append sb (str "    " (emit-fetch-and-page item diff?)))
                    (.append sb "        ;;\n"))
                  (.append sb "    esac\n"))))
            (.append sb "    ;;\n")))))
    (.append sb "esac\n")
    (spit dispatch-path (str sb))
    (.setExecutable (io/file dispatch-path) true)
    dispatch-path))

;; ---------------------------------------------------------------------------
;; Display
;; ---------------------------------------------------------------------------

(def ^:private loop-keys
  ;; Keys that exit fzf for Clojure-side handling. Pickers and marks use
  ;; execute+reload bindings instead — they don't appear here.
  #{"ctrl-u" "ctrl-/"})

(def ^:private expect-keys (str/join "," loop-keys))

(defn- status-header
  "Build the fzf --header string showing current sort and active filters."
  [sort-idx {:keys [types sources topics]}
   all-types all-sources all-topics]
  (let [active-types   (or types (set all-types))
        active-sources (or sources (set all-sources))
        active-topics  (or topics (set all-topics))]
    (str "[" (first (nth sort-options sort-idx)) "]"
         (when-not (= active-types (set all-types))
           (str " types:" (str/join "," (sort active-types))))
         (when-not (or (empty? all-sources) (= active-sources (set all-sources)))
           (str " src:" (str/join "," (sort active-sources))))
         (when-not (or (empty? all-topics) (= active-topics (set all-topics)))
           (str " topic:" (str/join "," (sort active-topics)))))))

(defn- column-headers
  "Return tab-joined header string for report columns."
  [show-type? show-src? skip]
  (str/join "\t"
            (concat
             (when-not (skip "mark")                     ["!"])
             (when (and show-type? (not (skip "type")))   ["Type"])
             (when (and show-src?  (not (skip "source"))) ["Source"])
             (when-not (skip "priority") ["P"])
             (when-not (skip "deadline") ["D"])
             (when-not (skip "flags")    ["Flags"])
             (when-not (skip "replies")  ["#"])
             (when-not (skip "author")   ["Author"])
             (when-not (skip "owner")    ["Owner"])
             (when-not (skip "date")     ["Date"])
             (when-not (skip "att")      ["Att"])
             ["Subject"])))

(defn- show-related!
  "Show related reports for the selected report in a nested fzf.
  Resolves related message-ids against the mid-index to display full report rows.
  Supports the same keybindings as the main fzf view."
  [selected-report mid-index config show-type? show-src? skip user-state]
  (when-let [rels (seq (:related selected-report))]
    (let [resolved (keep #(get mid-index (:message-id %)) rels)]
      (when (seq resolved)
        (let [resolved      (vec resolved)
              skip          (normalize-skip-columns skip)
              header        (column-headers show-type? show-src? skip)
              rows          (mapv #(report->row % show-type? show-src? skip user-state) resolved)
              aligned       (tabulate (cons header rows))
              input         (str/join "\n" aligned)
              ts            (System/currentTimeMillis)
              dispatch-path (tmp-path "related"      ts ".sh")
              help-path     (tmp-path "related-help" ts ".txt")]
          (spit help-path usage-text)
          (try
            (write-dispatch-script! dispatch-path help-path config resolved)
            (apply process/shell {:in input :out :string :continue true}
                   ["fzf" "--header-lines" "1"
                    "--header" (str "~ " (count resolved) " related to: "
                                    (truncate (:subject selected-report "(no subject)") 60)
                                    " [Ctrl-x: back]")
                    "--no-sort" "--reverse" "--no-hscroll"
                    "--prompt" "related~ "
                    "--bind" (str "enter:" exec-action "(" dispatch-path " open {n})")
                    "--bind" (str "ctrl-o:execute-silent(" dispatch-path " browse {n})")
                    "--bind" (str "ctrl-v:" exec-action "(" dispatch-path " view {n})")
                    "--bind" (str "ctrl-h:" exec-action "(" dispatch-path " help)")
                    "--bind" "ctrl-x:abort"
                    "--bind" "esc:abort"])
            true
            (finally
              (.delete (io/file dispatch-path))
              (.delete (io/file help-path)))))))))


(defn- handle-fzf-key
  "Handle an fzf --expect key (only ctrl-u and ctrl-/). Mutates the session
  file in place if needed; returns the cursor-pos to restore on next fzf
  start, or nil to reset."
  [key-used session-path
   & {:keys [selected-report sel-idx config show-type? show-src? skip-columns
             reload-fn]}]
  (case key-used
    "ctrl-/"
    (do (when (and selected-report (seq (:related selected-report)))
          (let [session (read-session session-path)
                mid-index (into {} (keep (fn [r]
                                           (when-let [mid (:message-id r)]
                                             [mid r])))
                                (:reports session))]
            ;; Silent no-op when none of the related ids resolve in the loaded
            ;; data: avoids a one-shot println that flashes between fzf alt-
            ;; screens. Same when the report has no :related at all.
            (show-related! selected-report mid-index config
                           show-type? show-src? skip-columns (load-state))))
        sel-idx)
    "ctrl-u"
    (do (if reload-fn
          (do (println "  Updating cache...")
              (update-sources-cache!)
              (let [session     (read-session session-path)
                    view        (or (:view-mode session) :default)
                    filtered    (startup-filter (reload-fn) (load-state) view)]
                (write-session! session-path
                                (assoc session
                                       :reports     (sort-reports filtered (:sort-idx session 0))
                                       :all-types   (vec (distinct
                                                          (map (comp display-type :type) filtered)))
                                       :all-sources (vec (distinct (keep :source filtered)))
                                       :all-topics  (vec (distinct (keep :topic filtered)))
                                       :types nil :sources nil :topics nil))))
          (println "  Cache update not available for this data source."))
        nil)
    nil))

(def ^:private bone-script-path
  (or (System/getProperty "babashka.file") *file*))

(defn display-reports!
  "Display reports interactively with fzf, or as plain text lines.
  reload-fn, when non-nil, is called on ctrl-u to refresh the cache and
  return a new {:reports ...} map.
  view-mode ∈ #{:default :todo :sticky :all} controls state-based visibility."
  [config reports & {:keys [reload-fn skip-columns view-mode]}]
  (let [show-type?   true
        show-src?    (multiple-sources? reports)
        skip-columns (normalize-skip-columns (or skip-columns (:skip-columns config)))
        view-mode    (or view-mode :default)
        ts            (System/currentTimeMillis)
        dispatch-path (tmp-path "dispatch" ts ".sh")
        session-path  (tmp-path "session"  ts ".json")
        help-path     (tmp-path "help"     ts ".txt")
        bb-bone       (str "bb " (shell-escape bone-script-path) " ")
        sess          (shell-escape session-path)
        mark-bind     (fn [key action]
                        (str key ":reload-sync(" bb-bone "--internal-print "
                             sess " {n} " action ")"))
        picker-bind   (fn [key sub]
                        ;; execute-silent keeps fzf from releasing its screen
                        ;; so the main list stays drawn while the tmux popup
                        ;; (or fallback inline picker) overlays it.
                        (str key ":execute-silent(" bb-bone sub " " sess
                             ")+reload-sync(" bb-bone "--internal-print " sess ")"))
        clear-bind    (str "ctrl-x:execute-silent(" bb-bone "--internal-clear-filters "
                           sess ")+reload-sync(" bb-bone "--internal-print " sess ")")]
    (if (empty? reports)
      (println "No reports found.")
      (if (fzf-available?)
        (try
          ;; Build initial session, write the help text once.
          (spit help-path usage-text)
          (let [filtered    (startup-filter reports (load-state) view-mode)
                all-types   (vec (distinct (map (comp display-type :type) filtered)))
                all-sources (vec (distinct (keep :source filtered)))
                all-topics  (vec (distinct (keep :topic filtered)))
                sorted      (sort-reports filtered 0)]
            (write-session! session-path
                            {:reports      sorted
                             :sort-idx     0
                             :types        nil :sources nil :topics nil
                             :all-types    all-types
                             :all-sources  all-sources
                             :all-topics   all-topics
                             :show-type?   show-type?
                             :show-src?    show-src?
                             :skip-columns skip-columns
                             :view-mode    view-mode}))
          (loop [cursor-pos nil]
            (let [user-state  (load-state)
                  session     (read-session session-path)
                  visible     (session-visible session user-state)
                  aligned     (session-render session user-state)
                  input       (str/join "\n" aligned)
                  _           (write-dispatch-script! dispatch-path help-path config visible)
                  fzf-args    (cond-> ["fzf" "--header-lines" "1"
                                       "--header" (status-header
                                                   (:sort-idx session 0) session
                                                   (:all-types session)
                                                   (:all-sources session)
                                                   (:all-topics session))
                                       "--no-sort" "--reverse" "--no-hscroll"
                                       "--prompt" "report> "
                                       "--expect" expect-keys
                                       "--bind" (str "enter:" exec-action "(" dispatch-path " open {n})")
                                       "--bind" (str "ctrl-o:execute-silent(" dispatch-path " browse {n})")
                                       "--bind" (str "ctrl-v:" exec-action "(" dispatch-path " view {n})")
                                       "--bind" (str "ctrl-h:" exec-action "(" dispatch-path " help)")
                                       "--bind" "ctrl-n:down"
                                       "--bind" "ctrl-p:up"
                                       "--bind" (mark-bind "alt-!" "todo")
                                       "--bind" (mark-bind "alt-*" "sticky")
                                       "--bind" (mark-bind "alt-enter" "read")
                                       "--bind" (picker-bind "ctrl-s" "--internal-pick-sort")
                                       "--bind" (picker-bind "ctrl-r" "--internal-pick-types")
                                       "--bind" (picker-bind "ctrl-b" "--internal-pick-sources")
                                       "--bind" (picker-bind "ctrl-t" "--internal-pick-topics")
                                       "--bind" clear-bind]
                                cursor-pos (conj "--bind" (str "load:pos(" (inc cursor-pos) ")")))
                  {:keys [exit out]}
                  (apply process/shell {:in input :out :string :continue true} fzf-args)
                  lines    (when (seq (str/trim out))
                             (str/split-lines (str/trim out)))
                  key-used (first lines)
                  selected (second lines)
                  sel-idx  (when selected
                             (some (fn [i] (when (= (nth aligned (inc i)) selected) i))
                                   (range (count visible))))
                  sel-report (when sel-idx (nth visible sel-idx))]
              (when (loop-keys key-used)
                (recur (handle-fzf-key key-used session-path
                                       :selected-report sel-report
                                       :sel-idx sel-idx
                                       :config config
                                       :show-type? show-type?
                                       :show-src? show-src?
                                       :skip-columns skip-columns
                                       :reload-fn reload-fn)))))
          (finally
            (.delete (io/file dispatch-path))
            (.delete (io/file session-path))
            (.delete (io/file help-path))))
        ;; Plain text fallback
        (let [user-state (load-state)
              visible    (filter #(visible-by-state? user-state % view-mode) reports)]
          (println (count visible) "report(s):\n")
          (doseq [r visible]
            (println " " (report->line r show-type? show-src? skip-columns user-state))))))))

;; ---------------------------------------------------------------------------
;; Report (triage summary)
;; ---------------------------------------------------------------------------

(def ^:private default-report-config
  {:sections    ["overview" "stale-patches" "stale-bugs" "active-threads" "expiring" "recent" "owned"]
   :stale-days  14
   :recent-days 7
   :expiry-days 7
   :top-n       10})

(defn- report-age-days
  "Days since report was posted. Returns nil on parse failure."
  [report]
  (let [ms (parse-date-ms (:date-raw report))]
    (when (pos? ms)
      (long (/ (- (System/currentTimeMillis) ms) 86400000)))))

(defn- section-header [title]
  (let [rule (apply str (repeat (max 0 (- 50 (count title) 3)) "─"))]
    (str "── " title " " rule)))

(defn- report-one-liner
  "Format a report as a concise one-line string for report output."
  [report & {:keys [prefix]}]
  (str (when prefix (format "%s  " prefix))
       (truncate (:subject report "(no subject)") 60)
       "  " (:from-name report (:from report "?"))))

(defn- section-overview [all-reports _rcfg]
  (let [open    (remove :closed all-reports)
        closed  (filter :closed all-reports)
        by-type (frequencies (map :type open))
        by-reason (frequencies (map #(:close-reason % "resolved") closed))
        flags   (map #(:flags (report-flags+score %)) open)
        acked   (count (filter #(= (first %) \A) flags))
        owned   (count (filter #(= (second %) \O) flags))
        neither (count (filter #(= % "---") flags))
        open-parts   (str/join ", " (for [[t n] (sort-by val > by-type)] (str (display-type t) ": " n)))
        closed-parts (str/join ", " (for [[r n] (sort-by val > by-reason)] (str r ": " n)))]
    (str (section-header "Overview") "\n"
         "  Open: " (count open) " | " open-parts "\n"
         "Closed: " (count closed) " | " closed-parts "\n"
         "Status: not acked: " neither ", acked: " acked ", owned: " owned "\n")))

(defn- section-stale [type-filter reports rcfg]
  (let [days   (:stale-days rcfg 14)
        top-n  (:top-n rcfg 10)
        label  (str "Stale " type-filter " (>" days " days, not acked, up to " top-n ")")
        stale  (->> reports
                    (filter #(= (:type %) type-filter))
                    (remove :acked)
                    (filter #(when-let [d (report-age-days %)] (> d days)))
                    (sort-by report-age-days >)
                    (take top-n))]
    (when (seq stale)
      (str (section-header label) "\n"
           (str/join "\n" (map #(report-one-liner % :prefix (format "%3dd" (report-age-days %))) stale))
           "\n"))))

(defn- section-active-threads [reports rcfg]
  (let [top-n  (:top-n rcfg 10)
        active (->> reports
                    (filter #(> (:replies % 0) 0))
                    (sort-by :replies >)
                    (take top-n))]
    (when (seq active)
      (str (section-header (str "Active threads (most replies, up to " top-n ")")) "\n"
           (str/join "\n" (map #(report-one-liner % :prefix (format "%3d replies" (:replies % 0))) active))
           "\n"))))

(defn- section-recent [reports rcfg]
  (let [days     (:recent-days rcfg 7)
        cutoff   (- (System/currentTimeMillis) (* days 86400000))
        recent   (->> reports
                      (filter #(> (parse-date-ms (:date-raw %)) cutoff))
                      (sort-by #(parse-date-ms (:date-raw %)) >))]
    (when (seq recent)
      (str (section-header (str "Recent (last " days " days)")) "\n"
           (str/join "\n" (map #(report-one-liner % :prefix (date-only (:date %))) recent))
           "\n"))))

(defn- section-expiring [reports rcfg]
  (let [days  (:expiry-days rcfg 7)
        top-n (:top-n rcfg 10)
        label (str "Expiring soon (within " days " days, up to " top-n ")")
        expiring (->> reports
                      (keep (fn [r]
                              (when-let [d (days-until r :expiry)]
                                (when (<= 0 d days)
                                  (assoc r ::exp-days d)))))
                      (sort-by ::exp-days)
                      (take top-n))]
    (when (seq expiring)
      (str (section-header label) "\n"
           (str/join "\n" (map #(report-one-liner % :prefix (format "%3dd left" (::exp-days %))) expiring))
           "\n"))))

(defn- section-owned [reports _rcfg addresses]
  (when (seq addresses)
    (let [addrs (if (set? addresses) addresses (normalize-addresses addresses))
          owned (->> reports
                     (remove :closed)
                     (filter #(when-let [o (:owned %)]
                                (contains? addrs (str/lower-case o))))
                     (sort-by #(parse-date-ms (:date-raw %)) >))]
      (when (seq owned)
        (str (section-header "Owned (your open reports)") "\n"
             (str/join "\n" (map #(report-one-liner % :prefix (date-only (:date %))) owned))
           "\n")))))

(def ^:private report-section-fns
  {"overview"       (fn [reports rcfg _addrs] (section-overview reports rcfg))
   "stale-patches"  (fn [reports rcfg _addrs] (section-stale "patch" reports rcfg))
   "stale-bugs"     (fn [reports rcfg _addrs] (section-stale "bug" reports rcfg))
   "active-threads" (fn [reports rcfg _addrs] (section-active-threads reports rcfg))
   "expiring"       (fn [reports rcfg _addrs] (section-expiring reports rcfg))
   "recent"         (fn [reports rcfg _addrs] (section-recent reports rcfg))
   "owned"          (fn [reports rcfg addrs]  (section-owned reports rcfg addrs))})

(defn- generate-report
  "Produce a triage report from loaded reports."
  [config reports]
  (let [rcfg      (merge default-report-config (:report config))
        addresses (normalize-addresses (:my-addresses config))
        sections  (:sections rcfg)
        open     (remove :closed reports)]
    (println)
    (doseq [s sections]
      (when-let [f (report-section-fns s)]
        (when-let [out (f (if (= s "overview") reports open) rcfg addresses)]
          (println out))))))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(def ^:private cli-spec
  {:file          {:alias :f :coerce :string :desc "Read reports from a JSON file" :ref "<FILE>"}
   :url           {:alias :u :coerce :string :desc "Fetch reports from a URL" :ref "<URL>"}
   :urls-file     {:alias :U :coerce :string :desc "Fetch reports from URLs listed in FILE" :ref "<FILE>"}
   :my-addresses  {:alias :M :coerce :string :desc "Override email address(es) (comma-separated)" :ref "<EMAILS>"}
   :source        {:alias :n :coerce :string :desc "Filter by source name" :ref "<NAME>"}
   :min-priority  {:alias :p :coerce :long   :desc "Filter by minimum priority (1-3)"}
   :min-score     {:alias :s :coerce :long   :desc "Filter by minimum score (0-7)"}
   :mine          {:alias :m :coerce :boolean :desc "Show only your reports"}
   :closed        {:alias :c :coerce :boolean :desc "Include closed reports"}
   :skip-columns  {:alias :S :coerce :string :desc "Hide columns (comma-separated)" :ref "<COLS>"}
   :todo          {:alias :T :coerce :boolean :desc "Show only items marked :todo"}
   :sticky        {:alias :Y :coerce :boolean :desc "Show items marked :todo or :sticky"}
   :all           {:alias :A :coerce :boolean :desc "Show all items, including :read"}
   :list-sources  {:alias :l :coerce :boolean :desc "List configured sources"}
   :add-source    {:alias :a :coerce :string :desc "Add a source" :ref "<URL>"}
   :remove-source {:alias :r :coerce :string :desc "Remove a source" :ref "<URL>"}
   :help          {:alias :h :coerce :boolean :desc "Show this help"}})

(defn- usage []
  (println "Usage: bone [COMMAND] [OPTIONS]")
  (println)
  (println "Commands:")
  (println "  update          Update the sources cache")
  (println "  clear           Clear the cache")
  (println "  report          Generate a triage summary")
  (println "  todo            Export marked items to ~/.config/bone/todo.org")
  (println)
  (println "Options:")
  (println (cli/format-opts {:spec cli-spec}))
  (println "  -               Read reports from stdin")
  (println)
  (println "Local marks (kept in ~/.config/bone/state.edn):")
  (println "  Alt-!  toggle :todo   (column '!')")
  (println "  Alt-*  toggle :sticky (column '*')")
  (println "  Alt-Enter toggle :read (column 'r'; hidden at next launch unless --all)")
  (println "  Ctrl-h inside fzf shows the full keymap."))

(defn- parse-opts
  "Parse CLI options from an argument sequence. Returns [cmd opts]."
  [args]
  (let [stdin?  (some #{"-"} args)
        args    (remove #{"-"} args)
        subcmds #{"clear" "update" "report" "todo"}
        cmd     (first (filter subcmds args))
        args    (remove subcmds args)
        opts    (cli/parse-opts args {:spec cli-spec})
        opts    (cond
                  stdin?          (assoc opts :data-src :stdin)
                  (:file opts)    (assoc opts :data-src :file)
                  (:url opts)     (assoc opts :data-src :url)
                  (:urls-file opts) (assoc opts :data-src :urls-file)
                  :else           opts)
        opts    (cond-> opts
                  (:my-addresses opts) (update :my-addresses #(str/split % #","))
                  (:skip-columns opts) (update :skip-columns #(str/split % #",")))]
    [cmd opts]))

(defn- validate-opts!
  "Validate parsed options. Throws on invalid values."
  [opts]
  (when (and (:min-priority opts) (not (#{1 2 3} (:min-priority opts))))
    (throw (ex-info (str "Invalid --min-priority: " (:min-priority opts) " (must be 1, 2, or 3)") {})))
  (when (and (:min-score opts) (not (<= 0 (:min-score opts) 7)))
    (throw (ex-info (str "Invalid --min-score: " (:min-score opts) " (must be 0–7)") {})))
  opts)

(defn- load-data
  "Load reports from the source specified in opts."
  [opts]
  (case (:data-src opts)
    :file      (load-from-file (:file opts))
    :url       (load-from-url (:url opts))
    :urls-file (load-from-urls-file (:urls-file opts))
    :stdin     (load-from-stdin)
    (load-from-sources)))

(defn- filter-reports
  "Apply CLI filters to a reports vector."
  [reports {:keys [source mine my-addresses min-priority min-score closed]}]
  (cond->> reports
    source                     (filter #(= (:source %) source))
    (and mine my-addresses)    (filter #(involves-email? % my-addresses))
    min-priority               (filter #(>= (:priority % 0) min-priority))
    min-score                  (filter #(>= (:score (report-flags+score %)) min-score))
    (not closed)               (remove :closed)))

(defn- enrich-opts
  "Validate and enrich parsed CLI options with email from config."
  [opts config]
  (let [opts  (validate-opts! opts)
        addrs (or (:my-addresses opts) (:my-addresses config))
        addrs (when addrs (if (string? addrs) [addrs] addrs))
        opts  (assoc opts :my-addresses addrs)]
    (when (and (:mine opts) (not (seq addrs)))
      (throw (ex-info "No address configured. Set :my-addresses in ~/.config/bone/config.edn or use -M EMAIL[,EMAIL,...]." {})))
    opts))

(defn -main [& args]
  ;; Internal subcommands — invoked by fzf bindings (reload/execute). Each
  ;; reads/updates the session file and exits.
  (case (first args)
    "--internal-print"
    (do (do-internal-print! (nth args 1) (nth args 2 nil) (nth args 3 nil))
        (System/exit 0))
    "--internal-pick-sort"
    (do (do-internal-pick-sort! (nth args 1)) (System/exit 0))
    "--internal-pick-types"
    (do (do-internal-pick-multi! (nth args 1) :types) (System/exit 0))
    "--internal-pick-sources"
    (do (do-internal-pick-multi! (nth args 1) :sources) (System/exit 0))
    "--internal-pick-topics"
    (do (do-internal-pick-multi! (nth args 1) :topics) (System/exit 0))
    "--internal-clear-filters"
    (do (do-internal-clear-filters! (nth args 1)) (System/exit 0))
    nil)
  (try
    (let [args       (seq (or (seq args) *command-line-args*))
          config     (load-config)
          [cmd opts] (parse-opts args)]
      (cond
        (:help opts)           (usage)
        (= cmd "clear")        (clear-cache!)
        (= cmd "update")       (update-sources-cache!)
        (= cmd "report")       (let [opts    (enrich-opts opts config)
                                      reports (filter-reports (:reports (load-data opts)) opts)
                                      cfg     (cond-> config (:my-addresses opts) (assoc :my-addresses (:my-addresses opts)))]
                                  (generate-report cfg reports))
        (= cmd "todo")         (let [[out n] (write-todo-org! todo-org-path)]
                                  (println (str "Wrote " out " (" n " todo"
                                                (when (not= n 1) "s") ")")))
        (:list-sources opts)   (list-sources!)
        (:add-source opts)     (add-source! (:add-source opts))
        (:remove-source opts)  (remove-source! (:remove-source opts))
        :else                  (let [opts      (enrich-opts opts config)
                                      reports   (filter-reports (:reports (load-data opts)) opts)
                                      reload-fn (when (nil? (:data-src opts))
                                                  #(filter-reports (:reports (load-from-sources)) opts))
                                      view-mode (cond (:all opts)    :all
                                                      (:todo opts)   :todo
                                                      (:sticky opts) :sticky
                                                      :else          :default)]
                                  (display-reports! config reports
                                                    :reload-fn reload-fn
                                                    :skip-columns (:skip-columns opts)
                                                    :view-mode view-mode))))
    (catch clojure.lang.ExceptionInfo e
      (binding [*out* *err*]
        (println (str "bone: " (.getMessage e))))
      (System/exit 1))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
