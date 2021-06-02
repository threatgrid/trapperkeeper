(ns puppetlabs.trapperkeeper.app
  (:require
   [clojure.core.async.impl.protocols :as async-prot]
            [puppetlabs.trapperkeeper.services :as s]
   [puppetlabs.trapperkeeper.util :refer [protocol]]
   [schema.core :as schema])
  (:import
   (clojure.lang IDeref)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Schema

(def TrapperkeeperAppOrderedServices
  [[(schema/one schema/Keyword "service-id")
    (schema/one (protocol s/Service) "Service")]])

(def TrapperkeeperAppContext
  "Schema for a Trapperkeeper application's internal context.  NOTE: this schema
  is intended for internal use by TK and may be subject to minor changes in future
  releases."
  {:service-contexts {schema/Keyword {schema/Any schema/Any}}
   :ordered-services TrapperkeeperAppOrderedServices
   :services-by-id          {schema/Keyword (protocol s/Service)}
   :lifecycle-channel       (protocol async-prot/Channel)
   :shutdown-channel        (protocol async-prot/Channel)
   :lifecycle-worker        (protocol async-prot/Channel)
   :shutdown-reason-promise IDeref})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; App Protocol

(defprotocol TrapperkeeperApp
  "Functions available on a trapperkeeper application instance"
  :extend-via-metadata true
  (get-service [this service-id] "Returns the service with the given service id")
  (service-graph [this] "Returns the prismatic graph of service fns for this app")
  (app-context [this] "Returns the application context for this app (an atom containing a map)")
  (check-for-errors! [this] (str "Check for any errors which have occurred in "
                                 "the bootstrap process.  If any have "
                                 "occurred, throw a `java.lang.Throwable` with "
                                 "the contents of the error.  If none have "
                                 "occurred, return the input parameter."))
  (init [this] "Initialize the services")
  (start [this] "Start the services")
  (stop [this] "Stop the services")
  (restart [this] "Stop and restart the services"))