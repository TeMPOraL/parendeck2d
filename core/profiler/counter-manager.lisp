(in-package #:parendeck2d.profiler)

;;; A centralized store for performance counters.


;;; State

(defvar *counters* (make-hash-table) "A hash table storing all existing counters in a (counter-name counter) -> counter mapping.")

(defun register-counter (&key name description interval (history-size +default-counter-history-size+))
  "Register a new coutner under given `NAME'."
  (log:info history-size)
  (setf (gethash name *counters*)
        (make-counter name description interval history-size)))

(defun get-counter (name &key (description "") (interval 0) (history-size +default-counter-history-size+))
  (let ((counter (gethash name *counters*)))
    (or counter
        (register-counter :name name :description description :interval interval :history-size history-size))))

(defun sample-appropriate-counters (current-time &optional sampling-time-designator)
  (maphash (lambda (name counter)
             (declare (ignore name))
             (when (counter-ripe-for-sampling-p counter current-time sampling-time-designator)
               (sample-counter counter current-time)))
           *counters*))

(defun clear-all-counters ()
  "Removes all managed counters."
  (clrhash *counters*))


;;; Counter reporter

(defun write-counter-report (filename)
  "Writes current state of all counters to a report file named `FILENAME'."
  (with-open-file (file filename :direction :output :if-exists :supersede)
    (who:with-html-output (file)
      (:html
       (write-report-header file)
       (:body
        (write-counters-summary file)
        (write-counters-full-data file)))))
  (log:info "Performance counters report written to file." filename))

(defun write-report-header (stream)
  (who:with-html-output (stream)
    (:head
     (:meta :charset "UTF-8")
     (:title "P2D performance counters report")
     ;; TODO helper styles and JS
     (:script :type "text/javascript" :src "https://www.gstatic.com/charts/loader.js")
     (:script :type "text/javascript" "google.charts.load('current', {packages: ['corechart', 'line', 'table']});
google.charts.setOnLoadCallback(drawAll);

function drawSingleChart(data, where, title, subtitle) {
  var chartData = google.visualization.arrayToDataTable([['nth sampling', 'increments', 'samples', 'spi']].concat(data));
  var options = {
    chart: {
      title: title,
      subtitle: subtitle
    },
    width: 900,
    height: 500,
    series: {
      0 : {targetAxisIndex: 0},
      1 : {targetAxisIndex: 1},
      2 : {targetAxisIndex: 2}
    },
    vAxes: {
      0: { title: 'Increments' },
      1: { title: 'Samples' },
      2: { title: 'Samples per increment' }
    },
    focusTarget: 'category',
    selectionMode: 'multiple'
  };

  var chart = new google.visualization.LineChart(document.getElementById(where));
  chart.draw(chartData, options);
}

function drawTable(data) {
  var tableData = new google.visualization.DataTable();
  tableData.addColumn('string', 'Counter name');
  tableData.addColumn('string', 'Counter description');
  tableData.addColumn('string', 'Sampling interval');
  tableData.addColumn('number', 'Increments min');
  tableData.addColumn('number', 'Increments running avg');
  tableData.addColumn('number', 'Increments max');
  tableData.addColumn('number', 'Samples min');
  tableData.addColumn('number', 'Samples running avg');
  tableData.addColumn('number', 'Samples max');
  tableData.addRows(data);

  var view = new google.visualization.DataView(tableData);
  var table = new google.visualization.Table(document.getElementById('counters-table'));
  table.draw(view);
}

function drawAll() {
drawTable(["              
              (maphash (lambda (name counter)
                         (who:fmt "['~A', '~A', '~A', ~F, ~F, ~F, ~F, ~F, ~F],"
                                  (package-qualify-symbol-for-title (counter-name counter))
                                  (counter-description counter)
                                  (describe-sampling-interval (counter-sampling-interval counter))
                                  (coerce (counter-increments-global-min counter) 'double-float)
                                  (coerce (counter-increments-running-avg counter) 'double-float)
                                  (coerce (counter-increments-global-max counter) 'double-float)
                                  (coerce (counter-samples-global-min counter) 'double-float)
                                  (coerce (counter-samples-running-avg counter) 'double-float)
                                  (coerce (counter-samples-global-max counter) 'double-float)))
                       *counters*)
              "]);
"
              (maphash (lambda (name counter)
                         (who:fmt "~&drawSingleChart(~A, '~A-chart', '~A', '~A');"
                                  (make-combined-json-dataset counter)
                                  (package-qualify-symbol-for-html-id (counter-name counter))
                                  (package-qualify-symbol-for-title (counter-name counter))
                                  (counter-description counter))) ;TODO escape
                       *counters*)
              "}"))))

(defun write-counters-summary (stream)
  (who:with-html-output (stream)
    (:div :class "counter-summary"
          (:h2 "Counters")
          (:div :id "counters-table"))))

(defun write-counters-full-data (stream)
  (who:with-html-output (stream)
    (maphash (lambda (name counter)
               (declare (ignore name))
               (write-counter-details stream counter))
             *counters*)))

(defun write-counter-details (stream counter)
  (who:with-html-output (stream)
    (:div :class "counter-report"
          (:h2 (who:str (package-qualify-symbol-for-title (counter-name counter))))
          (:p (who:esc (counter-description counter)))
          (:ul
           (:li (who:fmt "Buffer size: ~A" (counter-history-size counter)))
           (:li (who:str (describe-sampling-interval (counter-sampling-interval counter))))
           (:li "Last recorded value"
                (:ul (:li "Increments: " (who:str (counter-current-increments counter)))
                     (:li "Sample: " (who:str (counter-current-sample counter)))))
           (:li "Stats"
                ;; FIXME all those coerces will fail if relevant values are NIL
                (:ul (:li (who:fmt "Increments (min/running avg/max): ~A/~A/~A"
                                   (coerce (counter-increments-global-min counter) 'double-float)
                                   (coerce (counter-increments-running-avg counter) 'double-float)
                                   (coerce (counter-increments-global-max counter) 'double-float)))
                     (:li (who:fmt "Samples (min/running avg/max): ~A/~A/~A"
                                   (coerce (counter-samples-global-min counter) 'double-float)
                                   (coerce (counter-samples-running-avg counter) 'double-float)
                                   (coerce (counter-samples-global-max counter) 'double-float))))))
          (:div :id (concatenate 'string (package-qualify-symbol-for-html-id (counter-name counter)) "-chart"))
          ;; TODO bulk of data as exportable data
          (:div :id (concatenate 'string (package-qualify-symbol-for-html-id (counter-name counter)) "-data")))))

(defun make-combined-json-dataset (counter)
  (let ((last-n-samples (csrb-values (counter-last-n-samples counter)))
        (last-n-increments (csrb-values (counter-last-n-increments counter))))
    (with-output-to-string (str)
      (format str "[")
      (loop for n from 1
         for sample across last-n-samples
         for increment across last-n-increments
         do (format str "[~F, ~F, ~F, ~F]," n increment sample (if (= 0 sample) 0 (/ sample increment))))
      (format str "]"))))

(defun package-qualify-symbol-for-html-id (symbol)
  (format nil "~A--~A" (package-name (symbol-package symbol)) (symbol-name symbol)))

(defun package-qualify-symbol-for-title (symbol)
  (prin1-to-string symbol))

(defun describe-sampling-interval (sampling-interval)
  (if (numberp sampling-interval)
      (format nil "Sampling every ~A second~:P" sampling-interval)
      (format nil "Sampling every ~A" (string-downcase (string sampling-interval)))))
