#lang racket
(require racket/gui/base)
(require racket/draw)
(require racket/date)

(define transparent-brush  (new brush% [style 'transparent]))
(define blue-brush (new brush% [color "blue"]))
(define blue-pen (make-pen #:color "blue" #:width 2))
(define green-pen (make-pen #:color "green"))
(define 2pi (* pi 2))
(define pi/2 (/ pi 2))
(define pi/4 (/ pi 4))
(define pi/6 (/ pi 6))
(define pi/12 (/ pi 12))
(define pi/30 (/ pi 30))
(define pi/60 (/ pi 60))
(define 3pi/2 (* 3 pi/2))

; compute font size and return w h pair list
(define (fontsize adc txt font)
(let-values ([( w h d v)  (send adc get-text-extent txt font )  ])
  (list w h)
))

; format 2 digit with 0
(define (ft t )(~r t #:min-width 2 #:pad-string "0"))

; create bg gradiant
(define (bg-brush x y)
    (new brush%
         [gradient
          (new linear-gradient%
               [x0 0]
               [y0 (/ y 2)]
               [x1 (/ x 2)]
               [y1 0]
               [stops
                (list (list 0  (   make-object color% 255 0 0 0.3))
                      (list 0.5 (make-object color% 0 255 0 0.3))
                      (list 1   (make-object color% 0 0 255 0.3)))])]))

;  draw 12 lines + hour txt for theta in range(0, step=pi/6, length=12)
(define (draw-clock adc cx cy cr )
  (send adc set-font (make-font #:size (/ cr 9)  #:weight 'ultraheavy #:family 'decorative) )
  (send adc set-text-foreground "Black")
  (for ([a  (in-range (* 2 pi)  0 (-(/ pi 6)))]
        [i  (in-range 12 0 -1 )]  ; rotation is anti clock so reverse count
       )
    (let* ([hh  (if(> i 9)(- i 9) (+ 3 i))] 
          [barx0 (- cr (/ cr 3.5))]
          [barx1 (- cr (/ cr 4.6))]
          [tcr (- cr  (/ cr 8))]
          [fsize (fontsize adc(format "~a" hh )(send adc get-font))])
    (send adc  set-rotation 0)
    (let* (
           [tx (-(* tcr (cos a))(/ (car fsize) 2))]
           [ty (-(* tcr (sin a))(/ (cadr fsize) 2))]) 
     (send adc  draw-text (format "~a" hh) tx ty #f 0 )
     (send adc  set-rotation a)
     (send adc set-pen "blue" 3 'solid)
     (send adc  draw-line barx0  0 barx1 0))   
  )))

; draw drawneedles 
(define (drawneedle adc  hoursize angle)
     (send adc  set-rotation angle)
     (send adc set-pen "black" 2 'solid)
     (send adc  draw-line 0 0 hoursize 0)
     (let* ([p (new dc-path%)] ; prepare neddle path 
           [p1 (/ hoursize 3)]
           [p2 5] [p3 -5]
           [p4 (- hoursize p1) ])
        (send p move-to  0 0)
        (send p line-to  p1 p2)
        (send p line-to  p4 0)
       (send p line-to   p1 p3)
       (send p line-to 0 0)
        (send p close)
     (send adc set-brush "Gold" 'solid) 
     (send adc draw-path p)  )
  )
; draw seconds
(define (drawseconds adc  cr angle)
      (let* (
           [size1 (- cr  (/ cr 8) )]
           [size2 (- cr (/ cr 6) -10)])
     (send adc set-pen "red" 2 'solid)
     (send adc  set-rotation angle )
     (send adc  draw-line 0 0 size2 0) 
  ))

; event on paint
(define (do-paint canvas adc )
  (send adc set-smoothing 'smoothed)
   (let-values ([(cx cy)  (send canvas get-client-size)]) ; canvas size
     
    (let* ( [d (current-date)]
            [cmy (round (/ cy 2))]
            [cmx (round (/ cx 2))]
            [cr (if (> cy cx) cmx cmy)]
            [cr/2 (/ cr 2)]
            [cr/4 (/ cr 4)]
            [hoursize (/ cr 2.2)]
            [minutesize (/ cr 1.5)]
            [day (date-day d)] ; set all date time part
            [month (date-month d)]
            [year (date-year d)]
            [hour (date-hour d)]
            [minutes (date-minute d)]
            [seconds (date-second d)]
            [today  (format "~a/~a/~a" (ft day) (ft month) year) ]
            [hms  (format "~a:~a:~a" (ft hour) (ft minutes) (ft seconds)) ]
            [datesize  (fontsize adc today (send adc get-font))] ; date text size
            [hmssize  (fontsize adc hms(send adc get-font))]  ; hms text sier
            [datexpos (-(/ (car datesize) 2))] ; compute text x y pos 
            [dateypos  (-  (+ cr/4 (cadr datesize)))] 
            [hmsxpos (-(/ (car hmssize) 2))]
            [hmsypos   cr/4 ]            
            [hourangle (+ pi/2 (* (+ hour (/ minutes 90))  (- pi/6))) ] ;set hour angle adjust a few according past minutes
            [minuterangle (+ pi/2 (* minutes (- pi/30)))] ; set minute angle
            [secondangle  (+ pi/2 (* seconds (- pi/30)))] ; set second angle
            [bgcolor (bg-brush cx cy)]
          )
     (send adc set-origin cmx cmy)
     (send adc set-brush bgcolor)
     (send adc draw-ellipse(- cr) (- cr) (* 2 cr) (* 2 cr)  )
     (send adc set-pen blue-pen)
     (send adc set-brush blue-brush) 
     (send adc draw-ellipse -10 -10 20 20 ) ; draw center
     (send adc set-brush transparent-brush)
     (send adc draw-ellipse(- cr) (- cr) (* 2 cr) (* 2 cr)  ) ; draw external circle
     (draw-clock adc cmx cmy cr ) ; draw clock internal
     (drawneedle adc  hoursize hourangle) ; draw hour neddle 
     (drawneedle adc  minutesize minuterangle)  ; draw minutes neddle 
     (drawseconds adc cr secondangle) ; draw seconds neddle 
      (send adc  set-rotation 0 ) 
     (send adc draw-text hms hmsxpos hmsypos #f 0 ) ; draw digital hour
     (send adc draw-text today  datexpos dateypos #f 0 ) ; draw date
    ))
  (send adc flush)
  )

; force redraw clock
(define (redraw) (send canvas refresh))

; class myframe
(define myframe% (class frame% 
                   (super-new)) 
  )
;; main windows frame with canvas inside
(define mainframe (new myframe% [label "demo"][width 200][height 200]
                       ;[alignment (list 'center 'center)]
                       ))

(define vpanel (new vertical-panel%   [parent mainframe]
                    ;[style (list 'border)]
                    ;[alignment (list 'center 'center)]
                    ))

(define canvas (new canvas%	[parent vpanel]
                    ;[style (list 'vscroll 'hscroll )]
                    [paint-callback do-paint]))

(send mainframe show #t)

(define atimer (new timer%	 
   		[notify-callback redraw ]	 
   	 	))
; execute time each second
(send atimer start 1000)