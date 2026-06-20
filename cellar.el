;;; cellar.el --- Manage the cellar with Emacs and sqlite. -*- lexical-binding: t; -*-

;; Version: 0.1.1
;; URL: https://github.com/foxfriday/cellar.el
;; Package-Requires: ((emacs "29"))

;;; Commentary:
;; The package uses a `sqlite' database to manage a collection of wines in two
;; different locations.
;;
;; Note: the SQL queries use `concat_ws' which requires SQLite 3.44.0 or later.

;;; Code:

(require 'cl-lib)

;;; Customization

(defgroup cellar nil
  "Manage a wine cellar with sqlite."
  :group 'applications
  :prefix "cellar-")

(defcustom cellar-database "~/Repos/wiki/db/inventory.db"
  "The location of the database."
  :type 'file
  :group 'cellar)

(defcustom cellar-note-dir "~/Repos/wiki/docs/cellar/"
  "Directory for tasting note files."
  :type 'directory
  :group 'cellar)

;;; Internal Helpers

(defun cellar--open (db)
  "Open the database DB."
  (if (file-exists-p db)
      (sqlite-open db)
    (error "%s file does not exist" db)))

(defmacro cellar--with-db (db-var &rest body)
  "Open cellar database, bind to DB-VAR, execute BODY, close on exit."
  (declare (indent 1))
  `(let ((,db-var (cellar--open cellar-database)))
     (unwind-protect
         (progn (sqlite-execute ,db-var "PRAGMA foreign_keys = ON") ,@body)
       (sqlite-close ,db-var))))

;;; Tasting Note Helpers

(defun cellar--parse-note ()
  "Parse a tasting note buffer and return a plist of its data.
Reads `iid', `date', and the five score keys from the YAML
frontmatter only.  Returns a plist with :iid, :date, :appearance,
:aroma, :taste, :aftertaste, :impression, or nil if iid or date
cannot be parsed."
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      ;; Narrow to the YAML frontmatter so free-form prose in the body
      ;; cannot accidentally match a `key: value' line.
      (when (looking-at "^---[ \t]*\n")
        (let ((start (match-end 0)))
          (goto-char start)
          (when (re-search-forward "^---[ \t]*$" nil t)
            (narrow-to-region start (match-beginning 0)))))
      (cl-flet ((field (key pattern)
                  (goto-char (point-min))
                  (when (re-search-forward
                         (format "^%s: *%s" key pattern) nil t)
                    (match-string 1))))
        (let ((iid (field "iid" "\\([0-9]+\\)"))
              (date (field "date" "\\([0-9-]+\\)")))
          (when (and iid date)
            (cl-flet ((score (key)
                        (let ((v (field key "\\([0-9]+\\)")))
                          (and v (string-to-number v)))))
              (list :iid (string-to-number iid) :date date
                    :appearance (score "appearance")
                    :aroma (score "aroma")
                    :taste (score "taste")
                    :aftertaste (score "aftertaste")
                    :impression (score "impression")))))))))

(defun cellar--upsert-tasting (db data filename)
  "Upsert tasting DATA with FILENAME into the tastings table in DB."
  (sqlite-execute
   db
   "INSERT INTO tastings(iid, date, appearance, aroma, taste, aftertaste, impression, filename)
    VALUES(?, ?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT(filename) DO UPDATE SET
      date=excluded.date, appearance=excluded.appearance, aroma=excluded.aroma,
      taste=excluded.taste, aftertaste=excluded.aftertaste, impression=excluded.impression"
   (list (plist-get data :iid)
         (plist-get data :date)
         (plist-get data :appearance)
         (plist-get data :aroma)
         (plist-get data :taste)
         (plist-get data :aftertaste)
         (plist-get data :impression)
         filename)))

;;;###autoload
(defun cellar-sync-note ()
  "Sync the current tasting note buffer into the tastings table."
  (interactive)
  (unless (buffer-file-name)
    (error "Buffer is not visiting a file"))
  (let ((data (cellar--parse-note))
        (filename (file-name-nondirectory (buffer-file-name))))
    (unless data
      (error "Could not parse iid or date from note"))
    (cellar--with-db db
      (cellar--upsert-tasting db data filename))
    (message "Synced %s" filename)))

;;; Selection Helpers

(defun cellar--read-producer (db)
  "Prompt for country, region, and producer in DB.
Return a cons cell of (PRODUCER . REGION)."
  (let* ((countries (sqlite-select
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
                      WHERE country = ? AND region = ? ORDER BY producer"
                     (list country region)))
         (producer (completing-read "Producer: " producers)))
    (cons producer region)))

(defun cellar--select-from-rows (rows prompt)
  "Select an id from ROWS using PROMPT.
Each row is a list of (ID LABEL); return the ID for the chosen LABEL."
  (let ((choices (make-hash-table :size (safe-length rows) :test 'equal)))
    (dolist (row rows)
      (puthash (format "%s" (cadr row)) (car row) choices))
    (gethash (completing-read prompt choices) choices nil)))

(defun cellar--select-pid (db)
  "Select a producer id from the database DB."
  (let* ((pr (cellar--read-producer db))
         (rows (sqlite-select
                db
                "SELECT pid, concat_ws(' ',producer,region,subregion,
                                       vineyard,designation,varietal,category)
                 FROM producers WHERE producer = ? AND region = ?
                 ORDER BY subregion, vineyard"
                (list (car pr) (cdr pr)))))
    (cellar--select-from-rows rows "Bottle: ")))

(defun cellar--select-iid (db)
  "Select an inventory id from the database DB."
  (let* ((pr (cellar--read-producer db))
         (rows (sqlite-select
                db
                "SELECT iid, concat_ws(' ',vint,producer,subregion,vineyard,
                                       designation,varietal,category,btl)
                 FROM wines WHERE producer = ? AND region = ?
                 ORDER BY vint, subregion"
                (list (car pr) (cdr pr)))))
    (cellar--select-from-rows rows "Bottle: ")))

(cl-defun cellar--check-inventory (&key id db offsite-min home-min)
  "Return non-nil if wine ID in DB has enough bottles in stock.
Requires at least OFFSITE-MIN bottles offsite (oi) and HOME-MIN
bottles at home (hi).  Returns nil when ID does not exist."
  (let ((offsite-min (or offsite-min 0))
        (home-min (or home-min 0))
        (winv (car (sqlite-select
                    db
                    "SELECT oi, hi FROM inventory WHERE iid = ?"
                    (list id)))))
    (when winv
      (and (>= (car winv) offsite-min) (>= (cadr winv) home-min)))))

(defun cellar--check-prefix (prefix)
  "Return the number from the PREFIX."
  (if (or (listp prefix) (not prefix))
      1
    prefix))

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

(defun cellar--read-validated (given prompt label min max)
  "Return a validated number for an optional database field.
Use GIVEN when non-nil, otherwise read a number with PROMPT.  A
value of 0 (or empty input) means \"unset\" and returns nil.
Signal an error built from LABEL when the value is below MIN or
above MAX."
  (let ((n (or given (string-to-number (read-from-minibuffer prompt)))))
    (cond ((= n 0) nil)
          ((or (< n min) (> n max))
           (error "%s of %s is invalid" label n))
          (t n))))

;;; Core Operations

(cl-defun cellar--move (&key offsite insite consume iid)
  "Add OFFSITE, move INSITE from offsite and CONSUME from home.
If IID is nil, prompt user to select a wine."
  (cellar--with-db wdb
    ;; Moving INSITE bottles onsite needs that many offsite (oi); consuming
    ;; CONSUME bottles needs that many at home (hi).
    (let* ((iid (or iid (cellar--select-iid wdb)))
           (iok (cellar--check-inventory :id iid :db wdb
                                         :offsite-min insite :home-min consume)))
      (unless iok
        (error "Not enough inventory"))
      (when offsite
        (message "%s bottles added off site, %s database updates"
                 offsite
                 (sqlite-execute wdb
                                 "UPDATE inventory
                                  SET oi = oi + ?
                                  WHERE iid = ?"
                                 (list offsite iid))))
      (when insite
        (message "%s bottles moved in site, %s database updates"
                 insite
                 (sqlite-execute wdb
                                 "UPDATE inventory
                                  SET oi = oi - ?, hi = hi + ?
                                  WHERE iid = ?"
                                 (list insite insite iid))))
      (when consume
        (message "%s bottles consumed, %s database updates"
                 consume
                 (sqlite-execute wdb
                                 "UPDATE inventory
                                  SET hi = hi - ?, ci = ci + ?
                                  WHERE iid = ?"
                                 (list consume consume iid)))))))

;;; Interactive Commands

;;;###autoload
(defun cellar-add-offsite (number)
  "Add a NUMBER of bottles to the offsite inventory."
  (interactive "P")
  (let ((n (cellar--check-prefix number)))
    (if (< n 1)
        (error "Cannot add less than one bottle")
      (cellar--move :offsite n))))

;;;###autoload
(defun cellar-move-onsite (number)
  "Move a NUMBER of bottles onsite."
  (interactive "P")
  (let ((n (cellar--check-prefix number)))
    (if (< n 1)
        (error "Cannot move less than one bottle")
      (cellar--move :insite n))))

;;;###autoload
(defun cellar-consume (number)
  "Consume NUMBER of bottles from in house inventory."
  (interactive "P")
  (let ((n (cellar--check-prefix number)))
    (if (< n 1)
        (error "Cannot consume less than one bottle")
      (cellar--move :consume n))))

;;;###autoload
(defun cellar-note ()
  "Create a Markdown tasting note for a wine."
  (interactive)
  (unless (file-directory-p cellar-note-dir)
    (error "Note directory %s does not exist" cellar-note-dir))
  (let ((filepath (expand-file-name
                   (format-time-string "%Y-%m-%dT%H:%M.md")
                   cellar-note-dir)))
    (when (file-exists-p filepath)
      (error "Note file %s already exists" filepath))
    (cellar--with-db wdb
      (let* ((iid (or (cellar--select-iid wdb)
                      (user-error "No wine selected")))
             (row (car (sqlite-select
                        wdb
                        "SELECT vint, producer, subregion, vineyard,
                                designation, varietal, category, btl
                         FROM wines WHERE iid = ?"
                        (list iid))))
             (vint       (nth 0 row))
             (producer   (nth 1 row))
             (subregion  (nth 2 row))
             (vineyard   (nth 3 row))
             (designation (nth 4 row))
             (varietal   (nth 5 row))
             (category   (nth 6 row))
             (btl        (nth 7 row))
             (content
              (concat "---\n"
                      (format "title: %s %s\n" vint producer)
                      "keywords: [wine note]\n"
                      (format "iid: %s\n" iid)
                      (format "date: %s\n" (format-time-string "%Y-%m-%d"))
                      "appearance: 0  # /3\n"
                      "aroma: 0       # /5\n"
                      "taste: 0       # /5\n"
                      "aftertaste: 0  # /3\n"
                      "impression: 0  # /4\n"
                      "---\n\n"
                      (when subregion
                        (format "- Subregion: %s\n" subregion))
                      (when vineyard
                        (format "- Vineyard: %s\n" vineyard))
                      (when designation
                        (format "- Designation: %s\n" designation))
                      (format "- Varietal: %s\n" varietal)
                      (when category
                        (format "- Category: %s\n" category))
                      (format "- Size: %s ml\n" btl)
                      "\n## Notes\n\n")))
        (find-file filepath)
        (insert content)
        (goto-char (point-min))))))

;;;###autoload
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
  (cellar--with-db wdb
    (let* ((producer (cellar--set-prod-value wdb "Producer" producer))
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
      (message "%s database updates" (sqlite-execute wdb sql val)))))

;;;###autoload
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
         (alc (cellar--read-validated alc "Alcohol (e.g. 13.5): " "Alcohol" 0.0 100.0))
         (w0 (cellar--read-validated w0 "Window Start: " "Window start" 2000 2100))
         (w1 (cellar--read-validated w1 "Window End: " "Window end" 2000 2100)))
    (when (or (< vint 1900) (> vint 2050))
      (error "The vintage year %s is invalid" vint))
    (when (or (< btl 187) (> btl 3000))
      (error "Bottle size of %s is invalid" btl))
    (when (< oi 1)
      (error "Inventory of %s is invalid" oi))
    (cellar--with-db wdb
      (let ((pid (cellar--select-pid wdb)))
        (message "%s database updates"
                 (sqlite-execute wdb sql (list pid vint btl oi alc w0 w1)))))))

;;;###autoload
(cl-defun cellar-update-window (&key w0 w1 iid)
  "Update window (W0 to W1) for wine with id IID."
  (interactive)
  (let* ((sql "UPDATE inventory SET w0=?, w1=? WHERE iid=?")
         (w0 (cellar--read-validated w0 "Window Start: " "Window start" 2000 2100))
         (w1 (cellar--read-validated w1 "Window End: " "Window end" 2000 2100)))
    (cellar--with-db wdb
      (let ((iid (or iid (cellar--select-iid wdb))))
        (message "%s database updates"
                 (sqlite-execute wdb sql (list w0 w1 iid)))))))

;;;###autoload
(cl-defun cellar-update-alcohol (&key alc iid)
  "Update alcohol to ALC for wine with id IID."
  (interactive)
  (let* ((sql "UPDATE inventory SET alc=? WHERE iid=?")
         (alc (cellar--read-validated alc "Alcohol (e.g. 13.5): " "Alcohol" 0.0 100.0)))
    (cellar--with-db wdb
      (let ((iid (or iid (cellar--select-iid wdb))))
        (message "%s database updates"
                 (sqlite-execute wdb sql (list alc iid)))))))

;;;###autoload
(cl-defun cellar-add-bottle-annotation (&key note iid)
  "Add NOTE to bottle with id IID."
  (interactive)
  (let ((note (if note
                   note
                (read-from-minibuffer "Bottle note: "))))
    (cellar--with-db wdb
      (let ((iid (or iid (cellar--select-iid wdb))))
        (message "%s database updates"
                 (sqlite-execute wdb
                                 "INSERT INTO notes(iid, note) VALUES(?,?)"
                                 (list iid note)))))))

;;;###autoload
(defun cellar-sync-notes ()
  "Batch import all Markdown tasting notes into the tastings table.
Scans `cellar-note-dir' for *.md files, parses each one, and
upserts into the database.  Reports the number of notes synced."
  (interactive)
  (unless (file-directory-p cellar-note-dir)
    (error "Note directory %s does not exist" cellar-note-dir))
  (let ((files (directory-files cellar-note-dir t "\\.md\\'"))
        (count 0))
    (cellar--with-db db
      (dolist (f files)
        (with-temp-buffer
          (insert-file-contents f)
          (let ((data (cellar--parse-note))
                (filename (file-name-nondirectory f)))
            (when data
              (cellar--upsert-tasting db data filename)
              (setq count (1+ count)))))))
    (message "Synced %d tasting notes" count)))

;;; Browse Mode

(defvar-local cellar-list--filter nil
  "Current filter as (COLUMN . VALUE) or nil.")

(defun cellar-list--update-mode-line ()
  "Reflect the active filter (if any) in the mode line."
  (setq mode-line-process
        (when cellar-list--filter
          (format " [%s=%s]" (car cellar-list--filter)
                  (cdr cellar-list--filter))))
  (force-mode-line-update))

(defun cellar-list--entries ()
  "Fetch wine entries from database for tabulated list."
  (cellar--with-db db
    (let* ((where (when cellar-list--filter
                    (format " WHERE %s = ?" (car cellar-list--filter))))
           (sql (concat "SELECT iid, producer, region, subregion, vineyard, "
                        "varietal, designation, vint, tot, w0, w1 "
                        "FROM wines"
                        where
                        " ORDER BY producer, vint"))
           (params (when cellar-list--filter
                     (list (cdr cellar-list--filter))))
           (rows (sqlite-select db sql params)))
      (mapcar (lambda (row)
                (list (nth 0 row)
                      (vector
                       (format "%s" (nth 0 row))
                       (or (nth 1 row) "")
                       (or (nth 2 row) "")
                       (or (nth 3 row) "")
                       (or (nth 4 row) "")
                       (or (nth 5 row) "")
                       (or (nth 6 row) "")
                       (format "%s" (or (nth 7 row) ""))
                       (format "%s" (or (nth 8 row) ""))
                       (cond
                        ((and (nth 9 row) (nth 10 row))
                         (format "%s-%s" (nth 9 row) (nth 10 row)))
                        ((nth 9 row)
                         (format "%s-" (nth 9 row)))
                        (t "")))))
              rows))))

(defvar cellar-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'cellar-list-consume)
    (define-key map "m" #'cellar-list-move-onsite)
    (define-key map "a" #'cellar-list-add-offsite)
    (define-key map "w" #'cellar-list-update-window)
    (define-key map "A" #'cellar-list-update-alcohol)
    (define-key map "n" #'cellar-list-add-bottle-annotation)
    (define-key map "N" #'cellar-note)
    (define-key map "d" #'cellar-list-delete-bottle)
    (define-key map (kbd "RET") #'cellar-list-detail)
    (define-key map "f" #'cellar-list-filter)
    (define-key map "F" #'cellar-list-clear-filter)
    map))

(define-derived-mode cellar-list-mode tabulated-list-mode "Cellar"
  "Major mode for browsing wine cellar inventory.

\\{cellar-list-mode-map}"
  (setq tabulated-list-format
        [("iid" 5 t :right-align t)
         ("Producer" 25 t)
         ("Region" 15 t)
         ("Subregion" 15 t)
         ("Vineyard" 20 t)
         ("Varietal" 15 t)
         ("Designation" 12 t)
         ("Vint" 5 t)
         ("Tot" 4 t :right-align t)
         ("Window" 10 t)])
  (setq tabulated-list-entries #'cellar-list--entries)
  (setq tabulated-list-sort-key (cons "Producer" nil))
  (tabulated-list-init-header)
  (cellar-list--update-mode-line)
  (hl-line-mode 1))

;;;###autoload
(defun cellar-browse ()
  "Browse wine cellar inventory in a tabulated list."
  (interactive)
  (let ((buf (get-buffer-create "*Cellar*")))
    (with-current-buffer buf
      (cellar-list-mode)
      (tabulated-list-print t))
    (switch-to-buffer buf)))

(defun cellar-list--label (iid)
  "Return a human-readable \"VINT PRODUCER\" label for the row at point.
Falls back to IID when no entry is available."
  (let ((entry (tabulated-list-get-entry)))
    (if entry
        (let ((vint (aref entry 7))
              (producer (aref entry 1)))
          (format "%s %s (id %s)"
                  (if (string= vint "") "NV" vint)
                  producer iid))
      (format "id %s" iid))))

(defun cellar-list-consume (number)
  "Consume NUMBER bottles of wine at point."
  (interactive "P")
  (let ((iid (tabulated-list-get-id))
        (n (cellar--check-prefix number)))
    (unless iid (user-error "No wine at point"))
    (when (< n 1) (user-error "Number of bottles must be at least 1"))
    (when (yes-or-no-p
           (format "Consume %d bottle(s) of %s? " n (cellar-list--label iid)))
      (cellar--move :consume n :iid iid)
      (tabulated-list-revert))))

(defun cellar-list-move-onsite (number)
  "Move NUMBER bottles from offsite to home for wine at point."
  (interactive "P")
  (let ((iid (tabulated-list-get-id))
        (n (cellar--check-prefix number)))
    (unless iid (user-error "No wine at point"))
    (when (< n 1) (user-error "Number of bottles must be at least 1"))
    (when (yes-or-no-p
           (format "Move %d bottle(s) onsite for %s? " n (cellar-list--label iid)))
      (cellar--move :insite n :iid iid)
      (tabulated-list-revert))))

(defun cellar-list-add-offsite (number)
  "Add NUMBER bottles to offsite for wine at point."
  (interactive "P")
  (let ((iid (tabulated-list-get-id))
        (n (cellar--check-prefix number)))
    (unless iid (user-error "No wine at point"))
    (when (< n 1) (user-error "Number of bottles must be at least 1"))
    (when (yes-or-no-p
           (format "Add %d bottle(s) offsite for %s? " n (cellar-list--label iid)))
      (cellar--move :offsite n :iid iid)
      (tabulated-list-revert))))

(defun cellar-list-update-window ()
  "Update drinking window for wine at point."
  (interactive)
  (let ((iid (tabulated-list-get-id)))
    (unless iid (user-error "No wine at point"))
    (cellar-update-window :iid iid)
    (tabulated-list-revert)))

(defun cellar-list-update-alcohol ()
  "Update alcohol percentage for wine at point."
  (interactive)
  (let ((iid (tabulated-list-get-id)))
    (unless iid (user-error "No wine at point"))
    (cellar-update-alcohol :iid iid)
    (tabulated-list-revert)))

(defun cellar-list-add-bottle-annotation ()
  "Add a database note for wine at point."
  (interactive)
  (let ((iid (tabulated-list-get-id)))
    (unless iid (user-error "No wine at point"))
    (cellar-add-bottle-annotation :iid iid)
    (tabulated-list-revert)))

(defun cellar-list-delete-bottle ()
  "Delete the inventory entry at point."
  (interactive)
  (let ((iid (tabulated-list-get-id)))
    (unless iid (user-error "No wine at point"))
    (when (yes-or-no-p (format "Delete %s? " (cellar-list--label iid)))
      (cellar--with-db db
        (sqlite-execute db "DELETE FROM inventory WHERE iid = ?" (list iid)))
      (tabulated-list-revert))))

(defun cellar-list-detail ()
  "Show full details and notes for the wine at point."
  (interactive)
  (let ((iid (tabulated-list-get-id)))
    (unless iid (user-error "No wine at point"))
    (let* ((data (cellar--with-db db
                   (list (car (sqlite-select db
                                "SELECT vint, producer, region, subregion, vineyard,
                                        designation, varietal, category, btl, oi, hi,
                                        ci, alc, w0, w1, country
                                 FROM wines WHERE iid = ?" (list iid)))
                         (sqlite-select db
                           "SELECT note, created_at FROM notes
                            WHERE iid = ? ORDER BY created_at" (list iid)))))
           (wine (car data))
           (notes (cadr data))
           (name (or (nth 1 wine) "Unknown"))
           (buf (get-buffer-create
                 (format "*Cellar: %s %s*" name (or (nth 0 wine) "")))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Wine Details (iid: %s)\n" iid))
          (insert (make-string 40 ?=) "\n\n")
          (insert (format "Producer:    %s\n" (or (nth 1 wine) "")))
          (insert (format "Varietal:    %s\n" (or (nth 6 wine) "")))
          (insert (format "Vintage:     %s\n" (or (nth 0 wine) "")))
          (insert (format "Country:     %s\n" (or (nth 15 wine) "")))
          (insert (format "Region:      %s\n" (or (nth 2 wine) "")))
          (insert (format "Subregion:   %s\n" (or (nth 3 wine) "")))
          (insert (format "Vineyard:    %s\n" (or (nth 4 wine) "")))
          (insert (format "Designation: %s\n" (or (nth 5 wine) "")))
          (insert (format "Category:    %s\n" (or (nth 7 wine) "")))
          (insert (format "Bottle:      %s ml\n" (or (nth 8 wine) "")))
          (insert (format "Offsite:     %s\n" (or (nth 9 wine) 0)))
          (insert (format "Home:        %s\n" (or (nth 10 wine) 0)))
          (insert (format "Consumed:    %s\n" (or (nth 11 wine) 0)))
          (insert (format "Alcohol:     %s\n"
                          (if (nth 12 wine)
                              (format "%.1f%%" (nth 12 wine))
                            "")))
          (insert (format "Window:      %s\n"
                          (cond
                           ((and (nth 13 wine) (nth 14 wine))
                            (format "%s-%s" (nth 13 wine) (nth 14 wine)))
                           ((nth 13 wine)
                            (format "%s-" (nth 13 wine)))
                           (t ""))))
          (insert "\n")
          (if notes
              (progn
                (insert "Notes\n")
                (insert (make-string 40 ?-) "\n")
                (dolist (n notes)
                  (insert (format "[%s] %s\n" (or (nth 1 n) "") (nth 0 n)))))
            (insert "No notes.\n")))
        (goto-char (point-min)))
      (switch-to-buffer buf)
      (special-mode))))

(defun cellar-list-filter ()
  "Filter wines by country or region."
  (interactive)
  (let ((field (completing-read "Filter by: " '("country" "region") nil t)))
    (cellar--with-db db
      (let* ((values (sqlite-select db
                       (format "SELECT DISTINCT %s FROM producers ORDER BY %s"
                               field field)))
             (value (completing-read (format "%s: " (capitalize field)) values)))
        (setq cellar-list--filter (cons field value)))))
  (cellar-list--update-mode-line)
  (tabulated-list-revert))

(defun cellar-list-clear-filter ()
  "Clear any active filter."
  (interactive)
  (setq cellar-list--filter nil)
  (cellar-list--update-mode-line)
  (tabulated-list-revert))

(provide 'cellar)
;;; cellar.el ends here
