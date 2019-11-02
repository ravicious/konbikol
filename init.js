'use strict';

var app = Elm.Main.init({
  node: document.getElementById('app-container')
})

var pdfjsLib = window['pdfjs-dist/build/pdf']

app.ports.parsePdf.subscribe(function(file) {
  getPdfTextFromFile(file).then(function(textArray) {
    app.ports.extractedTextFromPdf.send(textArray)
  }, function(error) {
    if (error.name == 'InvalidPDFException') {
      // TODO: Tell Elm the file is invalid.
      console.error('Invalid PDF!')
    } else {
      console.error(error)
    }
  })
})

function getPdfTextFromFile(file) {
  return new Promise(function(resolve, reject) {
    var fileReader = new FileReader()

    fileReader.onload = function() {
      var typedArray = this.result
      var pdf = pdfjsLib.getDocument(typedArray).promise

      resolve(pdf.then(function(pdf) {
        return pdf.getPage(1)
      }).then(function(page) {
        return page.getTextContent()
      }).then(function(textContent) {
        return textContent.items.map(function (s) { return s.str })
      })
      )
    }

    fileReader.readAsArrayBuffer(file)
  })
}
