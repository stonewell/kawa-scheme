(define as-xml (make <gnu.kawa.xml.OutputAsXML>))

;;; Parse an XML file (specified by a URL or url string), giving a <document>.
(define (parse-xml-from-url url) :: <document>
  (invoke-static <gnu.xquery.util.Document> 'document url))

#|
(define (parse-nsxml-from-url url) :: <document>
  (if (not (instance? url <java.net.URL>))
      (set! url (make <java.net.URL> (invoke url 'toString))))
  (let* ((doc :: <document> (make <document>))
	 (parser :: <gnu.xml.XMLParser>
		 (make <gnu.xml.XMLParser> url
		       (make <gnu.xml.ParsedXMLToConsumer>
			 (make <gnu.xml.NamespaceResolver> doc)))))
    (invoke parser 'parse)
    doc))

(define (print-nsxml-from-url url #!optional (out (current-output-port)))
  (if (not (instance? url <java.net.URL>))
      (set! url (make <java.net.URL> (invoke url 'toString))))
  (let ((parser :: <gnu.xml.XMLParser>
		(make <gnu.xml.XMLParser> url
		      (make <gnu.xml.ParsedXMLToConsumer>
			(make <gnu.xml.NamespaceResolver>
			  (make <gnu.xml.XMLPrinter> out))))))
    (invoke parser 'parse)))
|#
