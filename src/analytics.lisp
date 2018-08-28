(defpackage #:ultralisp/analytics
  (:use #:cl)
  (:import-from #:cl-yandex-metrika)
  (:import-from #:ultralisp/variables
                #:get-yandex-counter-id
                #:get-google-counter-id)
  (:import-from #:weblocks/html
                #:with-html)
  (:export
   #:render-yandex-counter
   #:render-google-counter
   #:hit))
(in-package ultralisp/analytics)


(defparameter *yandex-template*
  "<!-- Yandex.Metrika counter -->
   <script type=\"text/javascript\" >
       (function (d, w, c) {
           (w[c] = w[c] || []).push(function() {
               try {
                   w.yaCounter = new Ya.Metrika({
                       id:~A,
                       clickmap:true,
                       trackLinks:true,
                       accurateTrackBounce:true,
                       webvisor:true
                   });
               } catch(e) { }
           });

           var n = d.getElementsByTagName(\"script\")[0],
               s = d.createElement(\"script\"),
               f = function () { n.parentNode.insertBefore(s, n); };
           s.type = \"text/javascript\";
           s.async = true;
           s.src = \"https://mc.yandex.ru/metrika/watch.js\";
   
           if (w.opera == \"[object Opera]\") {
               d.addEventListener(\"DOMContentLoaded\", f, false);
           } else { f(); }
       })(document, window, \"yandex_metrika_callbacks\");
   </script>
   <noscript><div><img src=\"https://mc.yandex.ru/watch/~A\" style=\"position:absolute; left:-9999px;\" alt=\"\" /></div></noscript>
   <!-- /Yandex.Metrika counter -->")


(defparameter *google-template*
  "<!-- Global site tag (gtag.js) - Google Analytics -->
   <script async src=\"https://www.googletagmanager.com/gtag/js?id=~A\"></script>
   <script>
     window.dataLayer = window.dataLayer || [];
     function gtag(){dataLayer.push(arguments);}
     gtag('js', new Date());
   
     gtag('config', '~A');
   </script>")


(defun render-yandex-counter ()
  (let ((counter-id (get-yandex-counter-id)))
    (when counter-id
      (with-html
        (:raw (format nil
                      *yandex-template*
                      counter-id
                      counter-id))))))


(defun render-google-counter ()
  (let ((counter-id (get-google-counter-id)))
    (when counter-id
      (with-html
        (:raw (format nil
                      *google-template*
                      counter-id
                      counter-id))))))


(defun hit (&rest args)
  (let ((counter-id (get-yandex-counter-id)))
    (when counter-id
      (let ((cl-yandex-metrika:*counter* counter-id))
        (apply #'cl-yandex-metrika:hit
               args)))))
