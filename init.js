'use strict';

var app = Elm.Main.init({
  node: document.getElementById('app-container')
})

var pdfjsLib = window['pdfjs-dist/build/pdf']
pdfjsLib.GlobalWorkerOptions.workerSrc = 'https://cdnjs.cloudflare.com/ajax/libs/pdf.js/2.2.228/pdf.worker.min.js'

app.ports.parsePdf.subscribe(function(file) {
  getPdfTextFromFile(file).then(console.log, function(error) {
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
        return textContent.items.map(function (s) { return s.str }).join("\n")
      })
      )
    }

    fileReader.readAsArrayBuffer(file)
  })
}
