(ns puppetlabs.trapperkeeper.shutdown-test
  (:require [clojure.test :refer :all]
            [puppetlabs.trapperkeeper.app :refer [app-context
                                                  check-for-errors!
                                                  get-service]]
            [puppetlabs.trapperkeeper.core :as tk]
            [puppetlabs.trapperkeeper.internal :as internal]
            [puppetlabs.trapperkeeper.services :refer [service service-id]]
            [puppetlabs.trapperkeeper.testutils.bootstrap :refer [bootstrap-services-with-empty-config]]
            [puppetlabs.trapperkeeper.testutils.logging :as logging]
            [puppetlabs.kitchensink.testutils.fixtures :refer [with-no-jvm-shutdown-hooks]]
            [schema.test :as schema-test]
            [puppetlabs.trapperkeeper.testutils.bootstrap :as testutils]
            [puppetlabs.trapperkeeper.app :as tk-app]))

(use-fixtures :once with-no-jvm-shutdown-hooks schema-test/validate-schemas)

(defprotocol ShutdownTestService
  :extend-via-metadata true)

(defprotocol ShutdownTestServiceWithFn
  :extend-via-metadata true
  (test-fn [this]))

(deftest shutdown-test
  (testing "service with shutdown hook gets called during shutdown"
    (let [shutdown-called?  (atom false)
          test-service      (service []
                              (stop [this context]
                                (reset! shutdown-called? true)
                                context))
          app               (bootstrap-services-with-empty-config [test-service])]
      (is (false? @shutdown-called?))
      (internal/shutdown! (app-context app))
      (is (true? @shutdown-called?)))))

(deftest shutdown-dep-order-test
  (testing "services are shut down in dependency order"
    (let [order       (atom [])
          service1    (service ShutdownTestService
                        []
                        (stop [this context]
                          (swap! order conj 1)
                          context))
          service2    (service [[:ShutdownTestService]]
                        (stop [this context]
                          (swap! order conj 2)
                          context))
          app         (bootstrap-services-with-empty-config [service1 service2])]
      (is (empty? @order))
      (internal/shutdown! (app-context app))
      (is (= @order [2 1])))))

(deftest shutdown-continue-after-ex-test
  (testing "services continue to shut down when one throws an exception"
    (let [shutdown-called?  (atom false)
          test-service      (service []
                              (stop [this context]
                                (reset! shutdown-called? true)
                                context))
          broken-service    (service []
                              (stop [this context]
                                (throw (RuntimeException. "dangit"))))
          app               (bootstrap-services-with-empty-config [test-service broken-service])]
      (is (false? @shutdown-called?))
      (logging/with-test-logging
        (internal/shutdown! (app-context app))
        (is (logged? #"Encountered error during shutdown sequence" :error)))
      (is (true? @shutdown-called?)))))

(deftest shutdown-run-app-test
  (testing "`tk/run-app` runs the framework (blocking until shutdown signal received), and `request-shutdown` shuts down services"
    (let [shutdown-called?  (atom false)
          test-service      (service []
                              (stop [this context]
                                (reset! shutdown-called? true)
                                context))
          app               (bootstrap-services-with-empty-config [test-service])
          shutdown-svc      (get-service app :ShutdownService)]
      (is (false? @shutdown-called?))
      (internal/request-shutdown shutdown-svc)
      (tk/run-app app)
      (is (true? @shutdown-called?)))))

(deftest shutdown-on-error-custom-fn-error-test
  (testing (str "`shutdown-on-error` in custom function causes services to be "
                "shut down and the error is rethrown from main")
    (let [shutdown-called?  (atom false)
          test-service      (service ShutdownTestServiceWithFn
                              [[:ShutdownService shutdown-on-error]]
                              (stop [this context]
                                (reset! shutdown-called? true)
                                context)
                              (test-fn [this]
                                (future (shutdown-on-error
                                         (service-id this)
                                         #(throw
                                           (Throwable.
                                            "oops"))))))
          app               (tk/boot-services-with-config [test-service] {})
          test-svc          (get-service app :ShutdownTestServiceWithFn)]
      (is (false? @shutdown-called?))
      (test-fn test-svc)
      (is (thrown-with-msg?
           Throwable
           #"oops"
           (tk/run-app app)))
      (is (true? @shutdown-called?)))))

(defn bootstrap-and-validate-shutdown
  [services shutdown-called? expected-exception-message]
  (let [app (tk/boot-services-with-config services {})]
    (is (thrown-with-msg?
         Throwable
         expected-exception-message
         (tk/run-app app))
        "tk run-app did not die with expected exception.")
    (is (true? @shutdown-called?)
        "Service shutdown was not called.")))

(deftest shutdown-ex-during-init-test
  (testing (str "shutdown will be called if a service throws an exception "
                "during init on main thread, with appropriate errors logged")
    (logging/with-test-logging
      (let [shutdown-called? (atom false)
            test-service     (service ShutdownTestService
                               []
                               (init [this context]
                                 (throw (Throwable. "oops"))
                                 context)
                               (stop [this context]
                                 (reset! shutdown-called? true)
                                 context))]
        (bootstrap-and-validate-shutdown
         [test-service]
         shutdown-called?
         #"oops")
        (is (logged? #"Error during service init!!!" :error)
            "Error message for service init not logged.")))))

(deftest shutdown-ex-during-start-test
  (testing (str "shutdown will be called if a service throws an exception "
                "during start on main thread, with appropriate errors logged")
    (logging/with-test-logging
      (let [shutdown-called?  (atom false)
            test-service      (service ShutdownTestService
                                []
                                (start [this context]
                                  (throw (Throwable. "oops"))
                                  context)
                                (stop  [this context]
                                  (reset! shutdown-called? true)
                                  context))]
        (bootstrap-and-validate-shutdown
         [test-service]
         shutdown-called?
         #"oops")
        (is (logged? #"Error during service start!!!" :error)
            "Error message for service start not logged.")))))

(deftest shutdown-log-errors-during-init-test
  (testing (str "`shutdown-on-error` will catch and log errors raised during "
                "init on main thread")
    (logging/with-test-logging
      (let [shutdown-called?  (atom false)
            test-service      (service ShutdownTestService
                                [[:ShutdownService shutdown-on-error]]
                                (init [this context]
                                  (shutdown-on-error
                                   :ShutdownTestService
                                   #(throw (Throwable. "oops")))
                                  context)
                                (stop [this context]
                                  (reset! shutdown-called? true)
                                  context))]
        (bootstrap-and-validate-shutdown
         [test-service]
         shutdown-called?
         #"oops")
        (is (logged? #"shutdown-on-error triggered because of exception!"
                     :error)
            "Error message for shutdown-on-error not logged.")))))

(deftest shutdown-log-errors-during-init-future-test
  (testing (str "`shutdown-on-error` will catch and log errors raised during "
                "init on future")
    (logging/with-test-logging
      (let [shutdown-called?  (atom false)
            test-service      (service ShutdownTestService
                                [[:ShutdownService shutdown-on-error]]
                                (init [this context]
                                  @(future
                                     (shutdown-on-error
                                      :ShutdownTestService
                                      #(throw (Throwable. "oops"))))
                                  context)
                                (stop [this context]
                                  (reset! shutdown-called? true)
                                  context))]
        (bootstrap-and-validate-shutdown
         [test-service]
         shutdown-called?
         #"oops")
        (is (logged? #"shutdown-on-error triggered because of exception!"
                     :error)
            "Error message for shutdown-on-error not logged.")))))

(deftest shutdown-log-errors-during-start-test
  (testing (str "`shutdown-on-error` will catch and log errors raised during"
                "start on main thread")
    (logging/with-test-logging
      (let [shutdown-called?  (atom false)
            test-service      (service ShutdownTestService
                                [[:ShutdownService shutdown-on-error]]
                                (start [this context]
                                  (shutdown-on-error
                                   :ShutdownTestService
                                   #(throw (Throwable. "oops")))
                                  context)
                                (stop [this context]
                                  (reset! shutdown-called? true)
                                  context))]
        (bootstrap-and-validate-shutdown
         [test-service]
         shutdown-called?
         #"oops")
        (is (logged? #"shutdown-on-error triggered because of exception!"
                     :error)
            "Error message for shutdown-on-error not logged.")))))

(deftest shutdown-log-errors-during-start-future-test
  (testing (str "`shutdown-on-error` will catch and log errors raised during "
                "start on future")
    (logging/with-test-logging
      (let [shutdown-called?  (atom false)
            test-service      (service ShutdownTestService
                                [[:ShutdownService shutdown-on-error]]
                                (start [this context]
                                  @(future
                                     (shutdown-on-error
                                      :ShutdownTestService
                                      #(throw (Throwable. "oops"))))
                                  context)
                                (stop [this context]
                                  (reset! shutdown-called? true)
                                  context))]
        (bootstrap-and-validate-shutdown
         [test-service]
         shutdown-called?
         #"oops")
        (is (logged? #"shutdown-on-error triggered because of exception!"
                     :error)
            "Error message for shutdown-on-error not logged.")))))

(deftest shutdown-on-error-callback-test
  (testing "`shutdown-on-error` takes an optional function that is called on error"
    (let [shutdown-called?    (atom false)
          on-error-fn-called? (atom false)
          broken-service      (service ShutdownTestServiceWithFn
                                [[:ShutdownService shutdown-on-error]]
                                (stop [this context]
                                  (reset! shutdown-called? true)
                                  context)
                                (test-fn [this]
                                  (shutdown-on-error (service-id this)
                                                     #(throw (RuntimeException. "uh oh"))
                                                     (fn [ctxt] (reset! on-error-fn-called? true)))))
          app                 (bootstrap-services-with-empty-config [broken-service])
          test-svc            (get-service app :ShutdownTestServiceWithFn)]
      (is (false? @shutdown-called?))
      (is (false? @on-error-fn-called?))
      (test-fn test-svc)
      (is (thrown-with-msg?
           RuntimeException
           #"uh oh"
           (tk/run-app app)))
      (is (true? @shutdown-called?))
      (is (true? @on-error-fn-called?)))))

(deftest shutdown-on-error-callback-error-test
  (testing "errors thrown by the `shutdown-on-error` optional on-error function are caught and logged"
    (let [broken-service  (service ShutdownTestServiceWithFn
                            [[:ShutdownService shutdown-on-error]]
                            (test-fn [this]
                              (shutdown-on-error (service-id this)
                                                 #(throw (Throwable. "foo"))
                                                 (fn [ctxt] (throw (Throwable. "busted on-error function"))))))
          app             (bootstrap-services-with-empty-config [broken-service])
          test-svc        (get-service app :ShutdownTestServiceWithFn)]
      (logging/with-test-logging
        (test-fn test-svc)
        (is (thrown-with-msg?
             Throwable
             #"foo"
             (tk/run-app app)))
        (is (logged? #"Error occurred during shutdown" :error))))))

(deftest shutdown-on-error-error-handling
  (testing "Shutdown-on-error should never throw an exception."
    (testing "providing `nil` for all arguments"
      (let [test-service (tk/service
                           [[:ShutdownService shutdown-on-error]]
                           (init [this context]
                             (shutdown-on-error nil nil nil)
                             context))]
        (is (not (nil? (tk/boot-services-with-config [test-service] {}))))))

    (testing "passing `nil` instead of a function"
      (let [test-service (tk/service
                           [[:ShutdownService shutdown-on-error]]
                           (init [this context]
                             (shutdown-on-error context nil)
                             context))]
        (is (not (nil? (tk/boot-services-with-config [test-service] {}))))))))

(deftest app-check-for-errors!-tests
  (testing "check-for-errors! throws exception for shutdown-on-error in init"
    (let [test-service     (service ShutdownTestService
                             [[:ShutdownService shutdown-on-error]]
                             (init [this context]
                               (shutdown-on-error
                                :ShutdownTestService
                                #(throw (Throwable. "oops")))
                               context))
          app              (tk/boot-services-with-config [test-service] {})]
      (is (thrown-with-msg?
           Throwable
           #"oops"
           (check-for-errors! app))
          "Expected error not thrown for check-for-errors!")))
  (testing "check-for-errors! throws exception for error in init"
    (let [test-service     (service ShutdownTestService
                             []
                             (init [this context]
                               (throw (Throwable. "oops"))
                               context))
          app              (tk/boot-services-with-config [test-service] {})]
      (is (thrown-with-msg?
           Throwable
           #"oops"
           (check-for-errors! app))
          "Expected error not thrown for check-for-errors!")))
  (testing "check-for-errors! throws exception for shutdown-on-error in start"
    (let [test-service     (service ShutdownTestService
                             [[:ShutdownService shutdown-on-error]]
                             (start [this context]
                               (shutdown-on-error
                                :ShutdownTestService
                                #(throw (Throwable. "oops")))
                               context))
          app              (tk/boot-services-with-config [test-service] {})]
      (is (thrown-with-msg?
           Throwable
           #"oops"
           (check-for-errors! app))
          "Expected error not thrown for check-for-errors!")))
  (testing "check-for-errors! throws exception for error in start"
    (let [test-service     (service ShutdownTestService
                             []
                             (start [this context]
                               (throw (Throwable. "oops"))
                               context))
          app              (tk/boot-services-with-config [test-service] {})]
      (is (thrown-with-msg?
           Throwable
           #"oops"
           (check-for-errors! app))
          "Expected error not thrown for check-for-errors!")))
  (testing "check-for-errors! returns app when no shutdown-on-error occurs"
    (let [test-service     (service ShutdownTestService
                             []
                             (init [this context]
                               context))
          app              (tk/boot-services-with-config [test-service] {})]
      (is (identical? app (check-for-errors! app))
          "app not returned for check-for-errors!"))))

(deftest shutdown-during-restart-test
  (testing "shutdown can't begin while restart or other lifecycle functions are in progress"
    (let [first-stop-begun? (promise)
          stop-should-proceed? (promise)
          lifecycle-events (atom [])
          svc (tk/service
                [[:ShutdownService request-shutdown]]
                (init [this context]
                  (swap! lifecycle-events conj :init)
                  context)
                (start [this context]
                  (swap! lifecycle-events conj :start)
                  context)
                (stop [this context]
                  (swap! lifecycle-events conj :stop)
                     ;; request shutdown, which will trigger the shutdown logic
                     ;; on the main thread.
                  (request-shutdown)
                  (deliver first-stop-begun? true)
                  @stop-should-proceed?
                  context))
          app (testutils/bootstrap-services-with-config
               [svc]
               {})
          ;; this ensures that the 'main' shutdown logic will be runnable,
          ;; and gives us a way to observe when it has completed.
          app-main-thread (future (tk/run-app app))
          shutdown-service (tk-app/get-service app :ShutdownService)]

      ;; no shutdown requested yet
      (is (nil? (internal/get-shutdown-reason shutdown-service)))

      ;; now we trigger a restart, which will call 'stop' for the first time,
      ;; which will request a shutdown but will block on the stop-should-proceed
      ;; promise
      (internal/restart-tk-apps [app])

      ;; wait until we know that the shutdown has been requested
      @first-stop-begun?

      ;; we want to give the main thread a little time in case it is going to
      ;; do anything nefarious (as it would have before we consolidated the
      ;; shutdown logic into the core.async worker loop)
      (Thread/sleep 100)

      ;; at this point, the app's shutdown-reason-promise should be set
      (is (not (nil? (internal/get-shutdown-reason shutdown-service))))

      ;; validate that the first three events are as expected (we're still blocked
      ;; in the first 'stop').
      (is (= [:init :start :stop] @lifecycle-events))

      ;; main thread should still be blocked
      (is (not (realized? app-main-thread)))

      ;; unblock the first 'stop'
      (deliver stop-should-proceed? true)
      ;; now wait for us to get all the way through the main thread
      @app-main-thread

      ;; validate that the restart completed before the shutdown called 'stop'
      ;; for a second time.
      (let [expected-lifecycle-events [:init :start :stop :init :start :stop]]
        (while (< (count @lifecycle-events) (count expected-lifecycle-events))
          (Thread/yield))
        (is (= expected-lifecycle-events @lifecycle-events))))))

(deftest shutdown-prioritized-test
  (testing "shutdown requests are prioritized over other pending lifecycle actions"
    (let [first-stop-begun? (promise)
          stop-should-proceed? (promise)
          lifecycle-events (atom [])
          svc (tk/service
                []
                (init [this context]
                  (swap! lifecycle-events conj :init)
                  context)
                (start [this context]
                  (swap! lifecycle-events conj :start)
                  context)
                (stop [this context]
                  (swap! lifecycle-events conj :stop)
                  (deliver first-stop-begun? true)
                  @stop-should-proceed?
                  context))
          app (testutils/bootstrap-services-with-config
               [svc]
               {})
          ;; this ensures that the 'main' shutdown logic will be runnable,
          ;; and gives us a way to observe when it has completed.
          app-main-thread (future (tk/run-app app))
          shutdown-service (tk-app/get-service app :ShutdownService)]

      ;; no shutdown requested yet
      (is (nil? (internal/get-shutdown-reason shutdown-service)))

      ;; now we trigger a restart, which will call 'stop' for the first time,
      ;; which will block on the stop-should-proceed promise
      (internal/restart-tk-apps [app])

      ;; wait until we know that the stop has begun
      @first-stop-begun?

      ;; request a few more restarts, which should be queued up
      (internal/restart-tk-apps [app])
      (internal/restart-tk-apps [app])

      ;; request a shutdown, which should supercede any of the queued restarts
      (internal/request-shutdown shutdown-service)

      ;; at this point, the app's shutdown-reason-promise should be set
      (is (not (nil? (internal/get-shutdown-reason shutdown-service))))

      ;; validate that the first three events are as expected (we're still blocked
      ;; in the first 'stop').
      (is (= [:init :start :stop] @lifecycle-events))

      ;; main thread should still be blocked
      (is (not (realized? app-main-thread)))

      ;; unblock the first 'stop'
      (deliver stop-should-proceed? true)
      ;; now wait for us to get all the way through the main thread
      @app-main-thread

      ;; validate that the first restart completed, and then we went directly to
      ;; the shutdown without processing the queued restarts.
      (let [expected-lifecycle-events [:init :start :stop :init :start :stop]]
        (is (= expected-lifecycle-events @lifecycle-events))))))

(deftest shutdown-called-twice-test
  (testing "app shuts down cleanly if shutdown is called twice"
    (let [first-stop-begun? (promise)
          stop-should-proceed? (promise)
          lifecycle-events (atom [])
          svc (tk/service
                []
                (init [this context]
                  (swap! lifecycle-events conj :init)
                  context)
                (start [this context]
                  (swap! lifecycle-events conj :start)
                  context)
                (stop [this context]
                  (swap! lifecycle-events conj :stop)
                  (deliver first-stop-begun? true)
                  @stop-should-proceed?
                  context))
          app (testutils/bootstrap-services-with-config
               [svc]
               {})
          ;; request a stop on two separate threads.  do these on futures
          ;; so that we can observe their progress.
          stop1-thread (future (tk-app/stop app))
          stop2-thread (future (tk-app/stop app))]

      ;; wait until we know that the stop has begun
      @first-stop-begun?

      ;; validate that the first three events are as expected (we're still blocked
      ;; in the first 'stop').
      (is (= [:init :start :stop] @lifecycle-events))

      ;; stop threads should both be blocked
      (is (not (realized? stop1-thread)))
      (is (not (realized? stop2-thread)))

      ;; unblock the first 'stop'
      (deliver stop-should-proceed? true)
      ;; now wait for both stop threads to complete
      @stop1-thread
      @stop2-thread

      ;; validate that the first stop completed, and that the second was a no-op
      (let [expected-lifecycle-events [:init :start :stop]]
        (is (= expected-lifecycle-events @lifecycle-events))))))
