(ns puppetlabs.trapperkeeper.services-namespaces-test.ns1)

(defprotocol FooService
  :extend-via-metadata true
  (foo [this]))