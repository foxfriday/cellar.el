;;; cellar.el --- Manage the cellar with Emacs and sqlite. -*- lexical-binding: t; -*-

;; Version: 0.0.01
;; URL: https://github.com/foxfriday/cellar
;; Package-Requires: ((emacs "29"))

;;; Commentary:
;; The package uses a `sqlite` data base to manage a collection of wines in two different
;; locations.

;;; Code:

(require 'cl-lib)

(defvar cellar-database "~/Repos/notes/cellar/data/inventory.db"
  "The location of the database.")

(defvar cellar-note-txt "** %s\n:PROPERTIES:\n:IID: %s\n:DT: %s\n:SCORE:\n:END:\n"
  "Text used for notes using name, id, and date.")

(defun cellar--open (db)
  "Open the database DB."
  (if (file-exists-p db)
      (sqlite-open db)
    (error (concat db " file does not exist."))))

(defun cellar--select-pid (db &optional columns)
  "Select an producer id and description from COLUMNS in the database DB."
  (let* ((cols (if columns
                   columns
                 "producer,region,subregion,vineyard,designation,varietal,category"))
         (countries (sqlite-select
                     db
                     "SELECT DISTINCT country FROM producers
                      ORDER BY country"))
         (country (completing-read "Country: " countries))
         (regions (sqlite-select
                   db
                   "SELECT DISTINCT region FROM producers
                    WHERE country = ? ORDER BY region"
                   (list country)))
         (region (completing-read "Region: " regions))
         (producers (sqlite-select
                     db
                     "SELECT DISTINCT producer FROM producers
                     WHERE region = ? ORDER BY producer"
                     (list region)))
         (producer (completing-read "Producer: " producers))
         (wines (sqlite-select
                 db
                 (concat "SELECT pid, concat_ws(' ',"
                         cols
                         ") FROM producers "
                         "WHERE producer = ? and region = ? "
                         "ORDER BY subregion, vineyard")
                 (list producer region)))
         (choices (make-hash-table :size (safe-length wines) :test 'equal)))
    (dolist (row wines)
      (puthash (format "%s" (car (cdr row))) (car row) choices))
    (gethash (completing-read "Bottle" choices) choices nil)))

(defun cellar--select-iid (db &optional columns)
  "Select an inventory id and description from COLUMNS in the database DB."
  (let* ((cols (if columns
                   columns
                 "vint,producer,subregion,vineyard,designation,varietal,category,btl"))
         (countries (sqlite-select
                     db
                     "SELECT DISTINCT country FROM producers
                      ORDER BY country"))
         (country (completing-read "Country: " countries))
         (regions (sqlite-select
                   db
                   "SELECT DISTINCT region FROM producers
                    WHERE country = ? ORDER BY region"
                   (list country)))
         (region (completing-read "Region: " regions))
         (producers (sqlite-select
                     db
                     "SELECT DISTINCT producer FROM producers
                     WHERE region = ? ORDER BY producer"
                     (list region)))
         (producer (completing-read "Producer: " producers))
         (wines (sqlite-select
                 db
                 (concat "SELECT iid, concat_ws(' ',"
                         cols
                         ") FROM wines "
                         "WHERE producer = ? and region = ? "
                         "ORDER BY vint, subregion")
                 (list producer region)))
         (choices (make-hash-table :size (safe-length wines) :test 'equal)))
    (dolist (row wines)
      (puthash (format "%s" (car (cdr row))) (car row) choices))
    (let ((desc (completing-read "Bottle" choices)))
      (list (gethash desc choices nil) desc))))

(cl-defun cellar--check-inventory (&key id db offsite insite)
  "Check OFFSITE and INSITE inventory for wine ID in database DB."
  (let ((omin (if offsite offsite 0))
        (hmin (if insite insite 0))
        (winv (car (sqlite-select
                    db
                    "SELECT oi, hi from inventory where iid = ?"
                    (list id)))))
    (if winv
        (and (>= (car winv) omin) (>= (car (cdr winv)) hmin))
      nil)))

(cl-defun cellar--move (&key offsite insite consume)
  "Add OFFSITE, move INSITE from offsite and CONSUME from home."
  (let* ((wdb (cellar--open cellar-database))
         (iid (car (cellar--select-iid wdb)))
         (iok (cellar--check-inventory :id iid :db wdb :insite consume :offsite insite)))
    (unless iok
      (error "Not enough inventory"))
    (if offsite
        (message "%s bottles added off site, %s database updates"
                 offsite
                 (sqlite-execute wdb
                                 "UPDATE inventory
                                  SET oi = oi + ?
                                  WHERE iid = ?"
                                 (list offsite iid))))
    (if insite
        (message "%s bottles moved in site, %s database updates"
                 insite
                 (sqlite-execute wdb
                                 "UPDATE inventory
                                  SET oi = oi - ?, hi = hi + ?
                                  WHERE iid = ?"
                                 (list insite insite iid))))
    (if consume
        (message "%s bottles consumed, %s database updates"
                 consume
                 (sqlite-execute wdb
                                 "UPDATE inventory
                                  SET hi = hi - ?, ci = ci + ?
                                  WHERE iid = ?"
                                 (list consume consume iid))))
    (sqlite-close wdb)))

(defun cellar--check-prefix (prefix)
  "Return the number from the PREFIX."
  (if (or (listp prefix) (not prefix))
      1
    prefix))

(defun cellar-add-offsite (number)
  "Add a NUMBER of bottles to the offsite inventory."
  (interactive "P")
  (let ((n (cellar--check-prefix number)))
    (if (< n 1)
        (error "Cannot add less than one bottle")
      (cellar--move :offsite n))))

(defun cellar-move-onsite (number)
  "Move a NUMBER of bottles onsite or offsite if negative."
  (interactive "P")
  (let ((n (cellar--check-prefix number)))
    (cellar--move :insite n)))

;;;###autoload
(defun cellar-consume (&optional number)
  "Consume NUMBER of bottles from in house inventory."
  (interactive)
  (let ((n (if number number 1)))
    (if (< n 1)
        (error "Cannot consume less than one bottle")
      (cellar--move :consume n))))

;;;###autoload
(defun cellar-note ()
  "Create a new note at point."
  (interactive)
  (let* ((wdb (cellar--open cellar-database))
         (btl (cellar--select-iid wdb))
         (txt cellar-note-txt))
    (when btl
      (insert (format txt
                      (car (cdr btl))
                      (car btl)
                      (format-time-string "%Y-%m-%d"))))))

;;;###autoload
(defun cellar-browse (&optional columns)
  "Browse wine inventory described by COLUMNS and place selection on kill ring."
  (interactive)
  (let* ((wdb (cellar--open cellar-database))
         (cols (if columns
                   columns
                 (concat "vint,producer,region,subregion,vineyard,designation,varietal,"
                         "category,'size:',btl,'total:',tot,'id:',iid")))
         (countries (sqlite-select
                     wdb
                     "SELECT DISTINCT country FROM producers
                      ORDER BY country"))
         (country (completing-read "Country: " countries))
         (regions (sqlite-select
                   wdb
                   "SELECT DISTINCT region FROM producers
                    WHERE country = ? ORDER BY region"
                   (list country)))
         (region (completing-read "Region: " regions))
         (wines (sqlite-select
                 wdb
                 (concat "SELECT concat_ws(' ',"
                         cols
                         ") FROM wines "
                         "WHERE region = ? "
                         "ORDER BY region, subregion, producer, vint")
                 (list region))))
    (sqlite-close wdb)
    (kill-new (completing-read "Bottle" wines))))

(defun cellar--set-prod-value (db column value)
  "Set the VALUE to insert for a COLUMN in the database DB."
  (if value
      value
    (let* ((sql (format "SELECT DISTINCT %s
                         FROM producers
                         WHERE %s IS NOT null
                         ORDER BY %s"
                        column column column))
           (ret (completing-read (concat column " (or null): ")
                                 (sqlite-select db sql))))
      (if (or (string= ret "") (string= ret "null")) nil ret))))

(cl-defun cellar-add-producer (&key producer
                                    varietal
                                    vineyard
                                    designation
                                    subregion
                                    region
                                    country
                                    category)
  "Add producer to database.

Values are PRODUCER, VARIETAL, VINEYARD, DESIGNATION, SUBREGION,
REGION, COUNTRY and CATEGORY."
  (interactive)
  (let* ((wdb (cellar--open cellar-database))
         (producer (cellar--set-prod-value wdb "Producer" producer))
         (varietal (cellar--set-prod-value wdb "Varietal" varietal))
         (vineyard (cellar--set-prod-value wdb "Vineyard" vineyard))
         (designation (cellar--set-prod-value wdb "Designation" designation))
         (subregion (cellar--set-prod-value wdb "Subregion" subregion))
         (region (cellar--set-prod-value wdb "Region" region))
         (country (cellar--set-prod-value wdb "Country" country))
         (category (cellar--set-prod-value wdb "Category" category))
         (val (list
               producer varietal vineyard designation subregion region country category))
         (sql "INSERT INTO producers(producer,varietal,vineyard,
                                     designation,subregion,region,country,category)
               VALUES(?,?,?,?,?,?,?,?)"))
    (message "%s database updates" (sqlite-execute wdb sql val))
    (sqlite-close wdb)))

(cl-defun cellar-add-bottle (&key vint btl oi alc w0 w1)
  "Add bottle to database.

Values are VINT, BTL, OI, ALC, W0, and W1."
  (interactive)
  (let* ((sql "INSERT INTO inventory(pid,vint,btl,oi,alc,w0,w1)
               VALUES(?,?,?,?,?,?,?)")
         (vint (if vint
                  vint
               (string-to-number (read-from-minibuffer "Vintage: "))))
         (btl (if btl
                  btl
                (string-to-number
                 (completing-read "Size: " (list "750" "375" "1500")))))
         (oi (if oi
                 oi
               (string-to-number
                (completing-read
                 "Quantity: "
                 (list "6" "12" "1" "2" "3" "4" "5" "7" "8" "9" "10" "11")))))
         (alc (if alc
                  alc
               (string-to-number (read-from-minibuffer "Alcohol (e.g. 135): "))))
         (w0 (if w0
                  w0
               (string-to-number (read-from-minibuffer "Window Start: "))))
         (w1 (if w1
                  w1
               (string-to-number (read-from-minibuffer "Window End: ")))))
    (if (or (< vint 1900) (> vint 2050))
        (error "The vintage year %s is invalid" vint))
    (if (or (< btl 187) (> btl 3000))
        (error "Bottle size of %s is invalid" btl))
    (if (< oi 1)
        (error "Inventory of %s is invalid" oi))
    (setq alc (if (= alc 0)
                  nil
                (if (or (< alc 50) (> alc 250))
                    (error "Alcohol of %s is invalid" alc)
                  alc)))
    (setq w0 (if (= w0 0)
                 nil
               (if (or (< w0 2000) (> w0 2100))
                   (error "Window start of %s is invalid" w0)
                 w0)))
    (setq w1 (if (= w1 0)
                 nil
               (if (or (< w1 2000) (> w1 2100))
                   (error "Window start of %s is invalid" w1)
                 w1)))
    (let* ((wdb (cellar--open cellar-database))
           (pid (cellar--select-pid wdb)))
      (message "%s database updates"
               (sqlite-execute wdb sql (list pid vint btl oi alc w0 w1)))
      (sqlite-close wdb))))

(cl-defun cellar-update-window (&key w0 w1 iid)
  "Update window (W0 to W1) for wine with id IID."
  (interactive)
  (let* ((sql "UPDATE inventory SET w0=?, w1=? WHERE iid=?")
         (w0 (if w0
                  w0
               (string-to-number (read-from-minibuffer "Window Start: "))))
         (w1 (if w1
                  w1
               (string-to-number (read-from-minibuffer "Window End: ")))))
    (setq w0 (if (= w0 0)
                 nil
               (if (or (< w0 2000) (> w0 2100))
                   (error "Window start of %s is invalid" w0)
                 w0)))
    (setq w1 (if (= w1 0)
                 nil
               (if (or (< w1 2000) (> w1 2100))
                   (error "Window start of %s is invalid" w1)
                 w1)))
    (let* ((wdb (cellar--open cellar-database))
           (iid (if iid iid(cellar--select-iid wdb))))
      (message "%s database updates"
               (sqlite-execute wdb sql (list w0 w1 iid)))
      (sqlite-close wdb))))

(cl-defun cellar-update-alcohol (&key alc iid)
  "Update alcohol to ALC for wine with id IID."
  (interactive)
  (let* ((sql "UPDATE inventory SET alc=? WHERE iid=?")
         (alc (if alc
                  alc
                (string-to-number (read-from-minibuffer "Alcohol (e.g. 135): ")))))
    (setq alc (if (= alc 0)
                  nil
                (if (or (< alc 50) (> alc 250))
                    (error "Alcohol of %s is invalid" alc)
                  alc)))
    (let* ((wdb (cellar--open cellar-database))
           (iid (if iid iid (car (cellar--select-iid wdb)))))
      (message "%s database updates"
               (sqlite-execute wdb sql (list alc iid)))
      (sqlite-close wdb))))

(cl-defun cellar-add-note (&key note iid)
  "Add NOTE to bottle with id IID, usually composition."
  (interactive)
  (let* ((sql "INSERT INTO notes(iid, note) values(?,?)")
         (note (if note
                  note
                (read-from-minibuffer "Bottle note: "))))
    (let* ((wdb (cellar--open cellar-database))
           (iid (if iid iid (car (cellar--select-iid wdb)))))
      (message "%s database updates"
               (sqlite-execute wdb sql (list iid note)))
      (sqlite-close wdb))))

(provide 'cellar)
;;; cellar.el ends here
