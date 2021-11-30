(defpackage hermes-agents.plotly-utils
  (:use #:cl)
  (:nicknames #:hsage.plotly-utils))
(in-package :hermes-agents.plotly-utils)

;; (loop repeat 1 collect (let* ((idx (random-int *rand-gen* 0 (length *rates*)))
;; 			       (input-dataset (get-input-dataset *rates* idx))
;; 			       (output-dataset (get-output-dataset *rates* idx)))
;; 			  (let ((result (ignore-errors
;; 					  (evaluate-agents (gen-agents 20 3 input-dataset) output-dataset))))
;; 			    (if result
;; 				(let ((revenues (assoccess result :revenues))
;; 				      (total-revenue (assoccess result :total-revenue))
;; 				      (avg-revenue (assoccess result :avg-revenue))
;; 				      (trades-won (assoccess result :trades-won))
;; 				      (trades-lost (assoccess result :trades-lost))
;; 				      (entry-times (mapcar #'time-to-unix (assoccess result :entry-times)))
;; 				      (exit-times (mapcar #'time-to-unix (assoccess result :exit-times)))
;; 				      (entry-prices (assoccess result :entry-prices))
;; 				      (exit-prices (assoccess result :exit-prices))
;; 				      (tps (assoccess result :tps))
;; 				      (sls (assoccess result :sls)))
;; 				  (format t "Total Revenue: ~a~%" total-revenue)
;; 				  (format t "Average Revenue: ~a~%~%" avg-revenue)
;; 				  (format t "Trades Won: ~a~%~%" trades-won)
;; 				  (format t "Trades Lost: ~a~%~%" trades-lost)

;; 				  (apply #'plotly-make-plot (plotly-layout)
;; 				       	 (append (list (plotly-candlestick output-dataset))
;; 				       		 (loop
;; 				       		    for revenue in revenues
;; 				       		    for entry-time in entry-times
;; 				       		    for exit-time in exit-times
;; 				       		    for entry-price in entry-prices
;; 				       		    for exit-price in exit-prices
;; 				       		    for tp in tps
;; 				       		    for sl in sls
;; 				       		    collect (when (> revenue 0)
;; 				       			      (plotly-line (list entry-time exit-time)
;; 				       					   (list entry-price (+ entry-price tp))
;; 				       					   "blue")))
;; 				       		 (loop
;; 				       		    for revenue in revenues
;; 				       		    for entry-time in entry-times
;; 				       		    for exit-time in exit-times
;; 				       		    for entry-price in entry-prices
;; 				       		    for tp in tps
;; 				       		    for sl in sls
;; 				       		    collect (when (> revenue 0)
;; 				       			      (plotly-line (list entry-time exit-time)
;; 				       					   (list entry-price (+ entry-price sl))
;; 				       					   "red" "dot")))
;; 				       		 (loop
;; 				       		    for revenue in revenues
;; 				       		    for entry-time in entry-times
;; 				       		    for exit-time in exit-times
;; 				       		    for entry-price in entry-prices
;; 				       		    for exit-price in exit-prices
;; 				  		    for tp in tps
;; 				       		    for sl in sls
;; 				       		    collect (when (< revenue 0)
;; 				       			      (plotly-line (list entry-time exit-time)
;; 				       					   (list entry-price (+ entry-price sl))
;; 				       					   "red")))
;; 				       		 (loop
;; 				       		    for revenue in revenues
;; 				       		    for entry-time in entry-times
;; 				       		    for exit-time in exit-times
;; 				       		    for entry-price in entry-prices
;; 				       		    for tp in tps
;; 				       		    for sl in sls
;; 				       		    collect (when (< revenue 0)
;; 				       			      (plotly-line (list entry-time exit-time)
;; 				       					   (list entry-price (+ entry-price tp))
;; 				       					   "blue" "dot")))))
;; 				  )
;; 				0))))

;; (let* ((idx 0)
;;        (training-results (reverse (loop
;; 				     for i from 0
;; 				     for result in *results*
;; 				     when (oddp i)
;; 				     collect (nth idx result))))
;;        (testing-results (let ((results (reverse (loop
;; 				    for i from 0
;; 				    for result in *results*
;; 				    when (evenp i)
;; 						   collect (nth idx result)))))
;; 			  (if (/= (length results) (length training-results))
;; 			      (butlast results)))))
;;   (format t "Correlation: ~a~%"
;;   	  (cl-mathstats:correlation training-results testing-results))
;;   (format t "Absolute Correlation: ~a~%"
;; 	  (let* (;; (mean-pos (mean (remove-if-not #'plusp training-results)))
;; 		 ;; (possible-good-predictions (remove-if-not (lambda (res) (> res mean-pos)) training-results))
;; 		 (possible-good-predictions (remove-if-not (lambda (res) (> res 0)) training-results))
;; 		 (good-predictions (reduce #'+ (mapcar (lambda (training testing)
;; 							 (if (and (> training 0)
;; 							      ;; (> training mean-pos)
;; 								  (> testing 0))
;; 							     1
;; 							     0))
;; 						       training-results
;; 						       testing-results))))
;; 	    (format nil "~a/~a (~2$%)" good-predictions
;; 		    (length possible-good-predictions)
;; 		    (* 100 (float (/ good-predictions (length possible-good-predictions)))))))
;;   (print (length training-results))
;;   (plotly-make-plot (plotly-layout)
;;   		    (plotly-line (iota (length training-results)) training-results "black" "dot")
;;   		    (plotly-line (iota (length testing-results)) testing-results "blue")
;;   		    )
;;   )

;; (defun generate-plot (plot-code width height)
;;   (let ((style (cl-css:css `((html :height 100%)
;;                              (body :height 100%
;;                                    :display flex
;;                                    :justify-content center
;;                                    :align-items center)
;;                              ("#plot" :width ,#?"${width}px"
;;                                       :height ,#?"${height}px")))))
;;     (who:with-html-output-to-string (_)
;;       (:html
;;        (:head
;;         (:script :src "https://cdn.plot.ly/plotly-latest.min.js")
;;         (:style (who:str style)))
;;        (:body
;;         (:div :id "plot")
;;         (:script (who:str plot-code)))))))

;; (defun open-plot (plot-code width height)
;;   "Write output to the file and open browser"
;;   (uiop/stream:with-temporary-file (:pathname pn :stream stream :direction :output :keep t :type "html")
;;     (write-string (generate-plot plot-code width height) stream)
;;     (sb-ext:run-program (or (uiop:getenv "BROWSER") "xdg-open") (list (namestring pn)) :wait nil :search t)))

;; (defun pl-plot (traces &key layout (width 1000) (height 700))
;;   "Plot the data (list of traces)"
;;   (let* ((json-traces (format nil "[~{~a~^,~}]" (mapcar #'json:encode-json-alist-to-string traces)))
;;          (json-layout (json:encode-json-alist-to-string layout))
;;          (plot-code (ps:ps
;;                       (let ((div ((ps:@ document get-element-by-id) "plot")))
;;                         (*plotly.plot div ((ps:@ *json* parse) (ps:lisp json-traces))
;;                                       ((ps:@ *json* parse) (ps:lisp json-layout)))))))
;;     (open-plot plot-code width height)))

(defun plot-poly (poly)
  (apply #'plotly-make-plot
         (plotly-layout)
         (loop for line in poly
               collect (let* ((x1 (min (-> line :x1) (-> line :x2)))
                              (x2 (max (-> line :x1) (-> line :x2)))
                              (y1 (if (= x1 (-> line :x1)) (-> line :y1) (-> line :y2)))
                              (y1 (if (= x2 (-> line :x1)) (-> line :y1) (-> line :y2))))
                         (plotly-line (list x1 x2)
                                      (list (-> line :y1)
                                            (-> line :y2)))))))

(defun plotly-layout ()
  `((:dragmode . "zoom")
    (:showlegend . ,*json-false*)
    (:xaxis . ((:rangeslider . ((:visible . ,*json-false*)))))
    (:yaxis . ((:fixedrange ,*json-false*)))
    (:title . "Trades")))

(defun plotly-candlestick (rates)
  (let ((x (loop for rate in rates collect (/ (read-from-string (assoccess rate :time)) 1000000)))
        (open (loop for rate in rates collect (assoccess rate :open-bid)))
        (high (loop for rate in rates collect (assoccess rate :high-bid)))
        (low (loop for rate in rates collect (assoccess rate :low-bid)))
        (close (loop for rate in rates collect (assoccess rate :close-bid))))
    `((:type . "candlestick")
      (:xaxis . "x")
      (:yaxis . "y")
      (:x . ,x)
      (:close . ,close)
      (:high . ,high)
      (:low . ,low)
      (:open . ,open))))

(defun plotly-line (xs ys &optional (color "red") (dash "solid"))
  `((:x . ,xs)
    (:y . ,ys)
    (:type . "scatter")
    (:line . ((:color . ,color)
              (:dash . ,dash)))))

(defun plotly-make-plot (layout &rest traces)
  (plotly-cl:pl-plot traces :layout layout :width 1700 :height 1000)
  (asdf:run-shell-command "mv ~a ~a" "/tmp/*.html" "~/"))
