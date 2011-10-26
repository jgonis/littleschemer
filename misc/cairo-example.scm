;; gtk-cairo-demo.scm:
;;   GTK/Cairo hello world
;;
;; Requirements:
;;   Linux:   libgtk-x11-2.0.so.0 libgdk-x11-2.0.so.0 libgobject-2.0.so.0 libcairo.so.2
;;   FreeBSD: libgtk-x11-2.0.so libgdk-x11-2.0.so libgobject-2.0.so libcairo.so
;;   OpenBSD: libgtk-x11-2.0.so libgdk-x11-2.0.so libgobject-2.0.so libcairo.so
;;   Darwin:  Gtk.framework Cairo.framework
;;   Windows: libgtk-win32-2.0-0.dll libgdk-win32-2.0-0.dll libgobject-2.0-0.dll libcairo-2.dll

(import (rnrs)
        (srfi :48)
        (ypsilon gtk constants)
        (ypsilon gtk init)
        (ypsilon gtk main)
        (ypsilon gtk window)
        (ypsilon gtk container)
        (ypsilon gtk drawing)
        (ypsilon gtk widget)
        (ypsilon gdk cairo)
        (ypsilon gdk drawable)
        (ypsilon gobject signal)
        (ypsilon cairo)
        (ypsilon ffi)
		(turtle))

(define NULL 0)
(define FALSE 0)

;;Jeff's Drawing Functions
(define create-white-background
  (lambda (cairo-context area-width area-height)
    (cairo_save cairo-context)
    (cairo_set_source_rgba cairo-context 1.0 1.0 1.0 1.0)
    (cairo_rectangle cairo-context 0.0 0.0
		     (c-int-ref area-width)
		     (c-int-ref area-height))
    (cairo_fill cairo-context)
    (cairo_restore cairo-context)))

(define draw-line-test
  (lambda (cairo-context)
    (cairo_save cairo-context)
    (cairo_set_source_rgba cairo-context 1.0 0.0 0.0 1.0)
    (cairo_move_to cairo-context 0.0 0.0)
    (cairo_line_to cairo-context 100.0 100.0)
    (cairo_stroke cairo-context)
    (cairo_restore cairo-context)))

(define drawing-func
  (lambda (cairo-context area-width area-height)
	(let ((t (turtle-create-default cairo-context)))
	  (create-white-background cairo-context area-width area-height)
	  (
	  ))


;;Boilerplate code
(define expose-event
  (signal-callback gboolean (GtkWidget* GdkEvent* gpointer)
    (lambda (drawing-area event data)
      (let ((window (gtk_widget_get_window drawing-area)))
        (let ((cairo-context (gdk_cairo_create window)))
          (let ((area-width (make-c-int 0)) (area-height (make-c-int 0)))
            ;; get area size
            (gdk_drawable_get_size window area-width area-height)
            ;; fill background
	    (drawing-func cairo-context area-width area-height)
            ;; cleanup
            (cairo_destroy cairo-context))))
      FALSE)))

(define destroy
  (signal-callback gboolean (GtkObject* gpointer)
    (lambda (obj data)
      (format #t "[destory ~s ~s]~%" obj data)
      (gtk_main_quit))))

(define run
  (lambda ()
    (gtk_init (vector (length (command-line)))
	      (apply vector (command-line)))
    (let ((drawing-area (gtk_drawing_area_new)))
      (gtk_widget_set_size_request drawing-area 640 480)
      (g_signal_connect drawing-area "expose-event" expose-event NULL)
      (let ((window (gtk_window_new GTK_WINDOW_TOPLEVEL)))
        (gtk_window_set_title window "Gtk/Cairo Hello World")
        (g_signal_connect window "destroy" destroy NULL)
        (gtk_container_add window drawing-area)
        (gtk_widget_show drawing-area)
        (gtk_widget_show window)
        (gtk_main)))))
;;Run the program that we have defined.
(run)


