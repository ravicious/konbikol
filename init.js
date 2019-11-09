'use strict';

var app = Elm.Main.init({
  node: document.getElementById('app-container')
})

var pdfjsLoadPromise = loadJsFile('vendor/pdf.min.js')

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
  return pdfjsLoadPromise.then(function() {
    var pdfjsLib = window['pdfjs-dist/build/pdf']
    return pdfjsLib.getDocument(input).promise
  }).then(function(pdf) {
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

var europeWarsawTimeZoneData = "BEGIN:VTIMEZONE\r\nTZID:Europe/Warsaw\r\nBEGIN:DAYLIGHT\r\nDTSTART:19770101T000000\r\nTZOFFSETFROM:+0100\r\nTZOFFSETTO:+0100\r\nRRULE:FREQ=YEARLY;BYDAY=1SA;BYMONTH=1\r\nTZNAME:CET\r\nEND:DAYLIGHT\r\nBEGIN:STANDARD\r\nDTSTART:19640927T010000\r\nTZOFFSETFROM:+0200\r\nTZOFFSETTO:+0100\r\nRRULE:FREQ=YEARLY;BYDAY=-1SU;BYMONTH=9\r\nTZNAME:CET\r\nEND:STANDARD\r\nEND:VTIMEZONE"

function addTimeZoneData(icsContent) {
  return icsContent.replace("VERSION:2.0\r\n", "VERSION:2.0\r\n" + europeWarsawTimeZoneData + "\r\n")
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
  var icsContent = addTimeZoneData(cal.build())
  link.href = "data:text/calendar;charset=utf8," + encodeURIComponent(icsContent)
  link.download = filename + ".ics"
  link.click()
}

function loadJsFile(url) {
  return new Promise(function(resolve, reject) {
    // Without the timeout, page rendering would still be blocked until the script gets loaded.
    setTimeout(function() {
      var element = document.createElement('script')

      element.src = url

      element.onload = function() {
        resolve(url)
      }
      element.onerror = function() {
        reject(url)
      }

      document.body.appendChild(element)
    }, 1)
  })
}
