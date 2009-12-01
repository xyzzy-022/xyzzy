
(require "ole")

(defun excel-demo ()
  (interactive)
  (let (app chart)
    (setq app (ole-create-object "Excel.Application"))
    (setf #{app.Visible} t)
    #{app.Workbooks.Add}
    (setf #{app.Range["a1"].Value} 3)
    (setf #{app.Range["a2"].Value} 2)
    (setf #{app.Range["a3"].Value} 1)
    #{app.Range["a1:a3"].Select}
    (setq chart #{app.Charts.Add})
    (setf #{chart.Type} -4100) ; xl3DColumn
    (do ((i 0 (+ i 10)))
	((> i 180))
      (setf #{chart.Rotation} i)
      (sit-for .1))
    (sit-for 1)
    #{app.ActiveWorkbook.Close[0]}
    #{app.Quit}))

(defun ie-demo ()
  (interactive)
  (let ((ie (ole-create-object "InternetExplorer.Application")))
    (setf #{ie.Visible} t)
    #{ie.Navigate["http://wakusei.cplaza.ne.jp/archiver/"]}))

