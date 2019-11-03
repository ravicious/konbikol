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

if (window.location.hash.includes("debug")) {
  getPdfTextFromPdfJsInput('ticket.pdf').then(function(textArray) {
    console.group("text array from pdf.js")
    console.log(textArray)
    console.groupEnd()

    app.ports.extractedTextFromPdf.send(textArray)
  }, console.error)
}

app.ports.downloadEvent.subscribe(function(event) {
  var cal = ics(event.uid, 'konbikol')
  var start = dateTimeToDate(event.start)
  var end = dateTimeToDate(event.end)

  cal.addEvent(event.subject, event.description, event.location, start, end)
  openCal(cal, event.subject)
})

function getPdfTextFromFile(file) {
  return new Promise(function(resolve, reject) {
    var fileReader = new FileReader()

    fileReader.onload = function() {
      var typedArray = this.result

      resolve(getPdfTextFromPdfJsInput(typedArray))
    }

    fileReader.readAsArrayBuffer(file)
  })
}

function getPdfTextFromPdfJsInput(input) {
  var pdf = pdfjsLib.getDocument(input).promise

  return pdf.then(function(pdf) {
    return pdf.getPage(1)
  }).then(function(page) {
    return page.getTextContent()
  }).then(function(textContent) {
    return textContent.items.map(function (s) { return s.str })
  })
}

function dateTimeToDate(dateTime) {
  return new Date(dateTime.year, dateTime.month - 1, dateTime.day, dateTime.hour, dateTime.min)
}

// `cal.download()` would always save the file, even on iOS.  So then I tried `window.open` with
// what's currently under `link.href`. That worked on iOS, but on the desktop it downloaded an
// unnamed .ics file.
// By using an <a> tag, you can add an event directly to a calendar on iOS and on the desktop it'll
// either ask to open the event in a calendar (Firefox) or just download the file (Safari, Chrome).
//
// https://stackoverflow.com/a/7034818/742872
function openCal(cal, filename) {
  var link = document.createElement("a")
  link.href = "data:text/calendar;charset=utf8," + encodeURIComponent(cal.build())
  link.download = filename + ".ics"
  link.click()
}
