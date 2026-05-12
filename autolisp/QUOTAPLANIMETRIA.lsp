;;; ================================================================
;;;  QUOTAPLANIMETRIA.lsp  |  v1.0  |  AutoCAD 2026
;;; ================================================================
;;;
;;;  Quotatura automatica di planimetrie 2D con un solo comando.
;;;
;;;  Il programma analizza LINE e LWPOLYLINE (e POLYLINE legacy),
;;;  estrae tutti i segmenti orizzontali e verticali, raccoglie i
;;;  punti significativi e genera automaticamente:
;;;
;;;    - Catena di quote orizzontali (sotto la planimetria)
;;;    - Quota totale orizzontale (più distante)
;;;    - Catena di quote verticali  (a sinistra della planimetria)
;;;    - Quota totale verticale (più distante)
;;;
;;;  INSTALLAZIONE:
;;;    Menu Gestisci > Applicazioni > Carica applicazione
;;;    oppure digitare: (load "percorso/QUOTAPLANIMETRIA.lsp")
;;;    oppure aggiungere al file acad.lsp / acaddoc.lsp
;;;
;;;  COMANDI DISPONIBILI:
;;;    QP                (alias breve)
;;;    QUOTAPLANIMETRIA  (nome completo)
;;;
;;;  CONFIGURAZIONE AVANZATA:
;;;    Prima di eseguire il comando, è possibile sovrascrivere
;;;    i valori predefiniti dalla console AutoLISP:
;;;
;;;      (setq *QP:OFF1* 3000.0)   ; cambia offset prima catena
;;;      (setq *QP:OFF2* 1500.0)   ; cambia offset quota totale
;;;      (setq *QP:LAYER* "DIM")   ; cambia nome layer
;;;      (setq *QP:MINSEG* 200.0)  ; ignora segmenti < 200 mm
;;;      (setq *QP:TOL* 2.0)       ; tolleranza H/V più permissiva
;;;
;;; ================================================================

;;; ----------------------------------------------------------------
;;;  VARIABILI GLOBALI DI CONFIGURAZIONE
;;;  Inizializzate solo se non già definite (permette override)
;;; ----------------------------------------------------------------

(if (null *QP:TOL*)    (setq *QP:TOL*    1.0))    ; tolleranza per classificare H/V
(if (null *QP:MINSEG*) (setq *QP:MINSEG* 100.0))  ; lunghezza minima segmento (mm)
(if (null *QP:OFF1*)   (setq *QP:OFF1*   2500.0))  ; offset dal bordo - catena singola
(if (null *QP:OFF2*)   (setq *QP:OFF2*   1200.0))  ; gap tra catena singola e quota totale
(if (null *QP:LAYER*)  (setq *QP:LAYER*  "QUOTE")) ; layer destinazione quote
(if (null *QP:COLOR*)  (setq *QP:COLOR*  3))       ; colore layer (3=verde ACI)
(if (null *QP:STYLE*)  (setq *QP:STYLE*  ""))      ; stile dimcota ("" = stile corrente)
(if (null *QP:SOUTH*)  (setq *QP:SOUTH*  T))       ; T = crea quote sotto (orizzontali)
(if (null *QP:WEST*)   (setq *QP:WEST*   T))       ; T = crea quote a sinistra (verticali)

;;; ================================================================
;;;  COMANDO PRINCIPALE
;;; ================================================================

(defun c:QUOTAPLANIMETRIA (/ *error*
                             _echo _osmode _clayer _dimstyle
                             ss segs segs-h segs-v n-diag
                             all-x all-y bbox
                             xmin xmax ymin ymax
                             off1 off2 lyr)

  ;;; ----- Gestione errori locale -----
  (defun *error* (msg)
    (setvar "CMDECHO"  _echo)
    (setvar "OSMODE"   _osmode)
    (setvar "CLAYER"   _clayer)
    (if (and _dimstyle (/= _dimstyle ""))
      (setvar "DIMSTYLE" _dimstyle))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*EXIT*,*QUIT*")))
      (princ (strcat "\n[QP] Errore: " msg)))
    (princ))

  ;;; ----- Salvataggio stato AutoCAD -----
  (setq _echo     (getvar "CMDECHO")
        _osmode   (getvar "OSMODE")
        _clayer   (getvar "CLAYER")
        _dimstyle (getvar "DIMSTYLE"))

  (setvar "CMDECHO" 0)
  (setvar "OSMODE"  0)

  ;;; ----- Banner -----
  (princ "\n")
  (princ "  *** QUOTATURA AUTOMATICA PLANIMETRIA - v1.0 ***\n")

  ;;; ----- Parametri interattivi con default -----
  (setq off1
    (progn
      (initget 6)  ; no zero, no negative
      (cond
        ((setq off1 (getdist
           (strcat "\nOffset prima catena di quote <"
                   (rtos *QP:OFF1* 2 0) ">: ")))
         off1)
        (t *QP:OFF1*))))

  (setq off2
    (progn
      (initget 6)
      (cond
        ((setq off2 (getdist
           (strcat "Spazio alla quota totale <"
                   (rtos *QP:OFF2* 2 0) ">: ")))
         off2)
        (t *QP:OFF2*))))

  (setq lyr
    (cond
      ((setq lyr (getstring
         (strcat "Nome layer quote <" *QP:LAYER* ">: ")))
       (if (= lyr "") *QP:LAYER* lyr))
      (t *QP:LAYER*)))

  ;;; ----- Selezione oggetti -----
  (princ "\nSelezionare gli oggetti della planimetria")
  (princ " (LINE, LWPOLYLINE, POLYLINE)...\n")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE"))))

  (if (null ss)
    (progn
      (princ "  [QP] Nessun oggetto selezionato. Uscita.")
      (*error* nil)
      (exit)))

  ;;; ----- Prepara layer per le quote -----
  (qp:mk-layer lyr *QP:COLOR*)
  (setvar "CLAYER" lyr)

  ;;; ----- Imposta stile dimensione se specificato -----
  (if (and *QP:STYLE*
           (/= *QP:STYLE* "")
           (tblsearch "DIMSTYLE" *QP:STYLE*))
    (setvar "DIMSTYLE" *QP:STYLE*))

  ;;; ----- Estrazione di tutti i segmenti -----
  (princ "\n  Analisi geometria...")
  (setq segs (qp:extract ss))

  (if (null segs)
    (progn
      (princ "\n  [QP] Nessun segmento valido trovato.")
      (*error* nil)
      (exit)))

  ;;; ----- Classificazione H / V / diagonale -----
  (setq segs-h nil  segs-v nil  n-diag 0)
  (foreach s segs
    (cond
      ((qp:horiz? s) (setq segs-h (cons s segs-h)))
      ((qp:vert?  s) (setq segs-v (cons s segs-v)))
      (t             (setq n-diag (1+ n-diag)))))

  (princ (strcat "\n  Segmenti orizzontali trovati: " (itoa (length segs-h))))
  (princ (strcat "\n  Segmenti verticali trovati:   " (itoa (length segs-v))))
  (if (> n-diag 0)
    (princ (strcat "\n  Segmenti diagonali ignorati:  " (itoa n-diag))))

  ;;; ----- Bounding box globale -----
  (setq bbox (qp:bbox segs)
        xmin (nth 0 bbox)
        ymin (nth 1 bbox)
        xmax (nth 2 bbox)
        ymax (nth 3 bbox))

  (princ (strcat "\n  Estensione X: " (rtos xmin 2 0) " ... " (rtos xmax 2 0)))
  (princ (strcat "\n  Estensione Y: " (rtos ymin 2 0) " ... " (rtos ymax 2 0)))

  ;;; ===== QUOTE ORIZZONTALI =====
  ;;; Raccoglie tutti i valori X dagli endpoint dei segmenti orizzontali,
  ;;; li deduplicati e crea una catena di DIMLINEAR orizzontali sotto
  ;;; la planimetria, più una quota totale ancora più in basso.

  (if (and segs-h *QP:SOUTH*)
    (progn
      (setq all-x (qp:endpoints-x segs-h))
      (setq all-x (vl-sort (qp:dedup all-x *QP:TOL*) '<))
      (if (>= (length all-x) 2)
        (progn
          (princ (strcat "\n  Creazione "
                         (itoa (1- (length all-x)))
                         " quote orizzontali..."))
          ;; Catena dimensioni individuali
          (qp:chain-h all-x ymin (- ymin off1))
          ;; Quota totale (da xmin a xmax)
          (qp:dim-h (car all-x) ymin
                    (last all-x) (- ymin (+ off1 off2))))
        (princ "\n  [QP] Punti X insufficienti per quote orizzontali."))))

  ;;; ===== QUOTE VERTICALI =====
  ;;; Raccoglie tutti i valori Y dagli endpoint dei segmenti verticali,
  ;;; li deduplicati e crea una catena di DIMLINEAR verticali a sinistra
  ;;; della planimetria, più una quota totale ancora più a sinistra.

  (if (and segs-v *QP:WEST*)
    (progn
      (setq all-y (qp:endpoints-y segs-v))
      (setq all-y (vl-sort (qp:dedup all-y *QP:TOL*) '<))
      (if (>= (length all-y) 2)
        (progn
          (princ (strcat "\n  Creazione "
                         (itoa (1- (length all-y)))
                         " quote verticali..."))
          ;; Catena dimensioni individuali
          (qp:chain-v all-y xmin (- xmin off1))
          ;; Quota totale (da ymin a ymax)
          (qp:dim-v (car all-y) xmin
                    (last all-y) (- xmin (+ off1 off2))))
        (princ "\n  [QP] Punti Y insufficienti per quote verticali."))))

  ;;; ----- Ripristino stato AutoCAD -----
  (setvar "CLAYER"   _clayer)
  (setvar "OSMODE"   _osmode)
  (setvar "DIMSTYLE" _dimstyle)
  (setvar "CMDECHO"  _echo)

  (princ "\n\n  *** Quotatura completata! ***")
  (princ "\n  Suggerimento: digita ZOOM E per vedere tutto il disegno.\n")
  (princ))

;;; Alias breve
(defun c:QP () (c:QUOTAPLANIMETRIA))


;;; ================================================================
;;;  HELPER: GESTIONE LAYER
;;; ================================================================

(defun qp:mk-layer (nm col)
  ;;; Crea il layer solo se non esiste già
  (if (null (tblsearch "LAYER" nm))
    (entmake
      (list '(0 . "LAYER")
            (cons 2 nm)
            '(70 . 0)
            (cons 62 col)
            '(6 . "Continuous")))))


;;; ================================================================
;;;  HELPER: ESTRAZIONE SEGMENTI
;;;  Ogni segmento è rappresentato come lista: (x1 y1 x2 y2)
;;; ================================================================

(defun qp:extract (ss / i en segs)
  ;;; Itera sulla selezione ed estrae segmenti da ogni entità
  (setq segs nil  i 0)
  (while (< i (sslength ss))
    (setq en (ssname ss i))
    (setq segs (append segs (qp:ent->segs en)))
    (setq i (1+ i)))
  ;;; Scarta i segmenti troppo corti (spigoli, nodi, etc.)
  (vl-remove-if
    '(lambda (s) (< (qp:len s) *QP:MINSEG*))
    segs))

(defun qp:ent->segs (en / ed tp)
  (setq ed (entget en)
        tp (cdr (assoc 0 ed)))
  (cond
    ((= tp "LINE")       (list (qp:line->seg ed)))
    ((= tp "LWPOLYLINE") (qp:lwpoly->segs ed))
    ((= tp "POLYLINE")   (qp:poly->segs en))
    (t nil)))

;;; LINE: legge i codici 10 (start) e 11 (end)
(defun qp:line->seg (ed / p1 p2)
  (setq p1 (cdr (assoc 10 ed))
        p2 (cdr (assoc 11 ed)))
  (list (car p1) (cadr p1) (car p2) (cadr p2)))

;;; LWPOLYLINE: raccoglie i vertici (codice 10) e costruisce segmenti
(defun qp:lwpoly->segs (ed / vs cls i p1 p2 segs)
  (setq vs   nil
        cls  (= (logand (cdr (assoc 70 ed)) 1) 1)
        segs nil)
  (foreach pr ed
    (if (= (car pr) 10)
      (setq vs (append vs (list (cdr pr))))))
  (setq i 0)
  (while (< i (1- (length vs)))
    (setq p1 (nth i vs)
          p2 (nth (1+ i) vs))
    (setq segs
      (cons (list (car p1) (cadr p1) (car p2) (cadr p2)) segs))
    (setq i (1+ i)))
  ;;; Segmento di chiusura se la polilinea è chiusa
  (if cls
    (progn
      (setq p1 (last vs)  p2 (car vs))
      (setq segs
        (cons (list (car p1) (cadr p1) (car p2) (cadr p2)) segs))))
  segs)

;;; POLYLINE (formato legacy): naviga le sub-entità VERTEX
(defun qp:poly->segs (en / ed sub cls segs fp pp pt)
  (setq ed   (entget en)
        cls  (= (logand (cdr (assoc 70 ed)) 1) 1)
        segs nil
        fp   nil
        pp   nil
        sub  (entnext en))
  (while (and sub
              (/= (cdr (assoc 0 (entget sub))) "SEQEND"))
    (setq ed (entget sub))
    (if (= (cdr (assoc 0 ed)) "VERTEX")
      (progn
        (setq pt (cdr (assoc 10 ed))
              pt (list (car pt) (cadr pt)))
        (if (null fp) (setq fp pt))
        (if pp
          (setq segs
            (cons (list (car pp) (cadr pp) (car pt) (cadr pt))
                  segs)))
        (setq pp pt)))
    (setq sub (entnext sub)))
  (if (and cls fp pp)
    (setq segs
      (cons (list (car pp) (cadr pp) (car fp) (cadr fp)) segs)))
  segs)


;;; ================================================================
;;;  HELPER: GEOMETRIA E CLASSIFICAZIONE
;;; ================================================================

(defun qp:len (s)
  (distance (list (nth 0 s) (nth 1 s))
            (list (nth 2 s) (nth 3 s))))

(defun qp:horiz? (s)
  ;;; Segmento orizzontale se |Δy| < tolleranza
  (< (abs (- (nth 3 s) (nth 1 s))) *QP:TOL*))

(defun qp:vert? (s)
  ;;; Segmento verticale se |Δx| < tolleranza
  (< (abs (- (nth 2 s) (nth 0 s))) *QP:TOL*))

(defun qp:bbox (segs / xmn ymn xmx ymx s)
  (setq xmn 1e38  ymn 1e38  xmx -1e38  ymx -1e38)
  (foreach s segs
    (setq xmn (min xmn (nth 0 s) (nth 2 s))
          ymn (min ymn (nth 1 s) (nth 3 s))
          xmx (max xmx (nth 0 s) (nth 2 s))
          ymx (max ymx (nth 1 s) (nth 3 s))))
  (list xmn ymn xmx ymx))

(defun qp:endpoints-x (segs / pts)
  ;;; Raccoglie tutti i valori X (x1 e x2) dei segmenti
  (setq pts nil)
  (foreach s segs
    (setq pts (cons (nth 0 s) (cons (nth 2 s) pts))))
  pts)

(defun qp:endpoints-y (segs / pts)
  ;;; Raccoglie tutti i valori Y (y1 e y2) dei segmenti
  (setq pts nil)
  (foreach s segs
    (setq pts (cons (nth 1 s) (cons (nth 3 s) pts))))
  pts)

(defun qp:dedup (vals tol / res v ok)
  ;;; Rimuove valori numerici duplicati entro la tolleranza tol
  (setq res nil)
  (foreach v vals
    (setq ok t)
    (foreach r res
      (if (< (abs (- r v)) tol)
        (setq ok nil)))
    (if ok (setq res (cons v res))))
  res)


;;; ================================================================
;;;  HELPER: CREAZIONE QUOTE LINEARI
;;; ================================================================

(defun qp:dim-h (x1 y x2 dim-y)
  ;;; DIMLINEAR orizzontale: misura la distanza X tra x1 e x2.
  ;;; La linea di quota viene posizionata a Y = dim-y.
  ;;; Il punto DIM viene posto al centro in X per forza il tipo H.
  (if (> (abs (- x2 x1)) *QP:TOL*)
    (command "._DIMLINEAR"
             (list x1 y 0.0)
             (list x2 y 0.0)
             (list (/ (+ x1 x2) 2.0) dim-y 0.0))))

(defun qp:dim-v (y1 x y2 dim-x)
  ;;; DIMLINEAR verticale: misura la distanza Y tra y1 e y2.
  ;;; La linea di quota viene posizionata a X = dim-x.
  (if (> (abs (- y2 y1)) *QP:TOL*)
    (command "._DIMLINEAR"
             (list x y1 0.0)
             (list x y2 0.0)
             (list dim-x (/ (+ y1 y2) 2.0) 0.0))))

(defun qp:chain-h (sorted-x y dim-y / i)
  ;;; Crea una catena continua di quote orizzontali tra valori X consecutivi
  (setq i 0)
  (while (< i (1- (length sorted-x)))
    (qp:dim-h (nth i sorted-x) y (nth (1+ i) sorted-x) dim-y)
    (setq i (1+ i))))

(defun qp:chain-v (sorted-y x dim-x / i)
  ;;; Crea una catena continua di quote verticali tra valori Y consecutivi
  (setq i 0)
  (while (< i (1- (length sorted-y)))
    (qp:dim-v (nth i sorted-y) x (nth (1+ i) sorted-y) dim-x)
    (setq i (1+ i))))


;;; ================================================================
;;;  MESSAGGIO DI CARICAMENTO
;;; ================================================================

(princ
  (strcat
    "\n  [QUOTAPLANIMETRIA v1.0] Caricato con successo."
    "\n  Comandi:  QP   oppure   QUOTAPLANIMETRIA"
    "\n  Configurazione: (setq *QP:OFF1* 3000) (setq *QP:LAYER* \"DIM\")\n"))
(princ)

;;; ================================================================
;;;  EOF - QUOTAPLANIMETRIA.lsp
;;; ================================================================
