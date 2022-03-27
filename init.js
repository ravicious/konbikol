'use strict';

var app = Elm.Main.init({
  node: document.getElementById('app-container')
})

// Start preloading pdf.min.js after page load.
// It's actually good that it's loaded at this point, the first big chunk of dependencies can be
// preloaded while the user selects the PDF file.
var pdfjsLoadPromise = loadJsFile('vendor/pdf.min.js')

app.ports.parsePdf.subscribe(function(args) {
  const [index, file] = args

  getPdfTextFromFile(file).then(function(textArray) {
    app.ports.extractedTextFromPdf.send([index, file.name, textArray])
  }, function(error) {
    console.error(error)
    let message

    if (error.name === "InvalidPDFException") {
      message = "Not a valid PDF file"
    } else if (error.message) {
      message = error.name + " (" + error.message + ")"
    } else {
      message = error.name
    }

    app.ports.pdfjsErrors.send([index, {fileName: file.name, message}])
  })
})

if (window.location.hash.includes("debug")) {
  (async () => {
    const response = await fetch('ticket.pdf')
    const blob = await response.blob()
    const file = new File([blob], 'ticket.pdf', {type: "application/pdf"}, 'utf-8')
    const container = new DataTransfer()
    const input = document.getElementById('pdf-file')

    container.items.add(file)
    input.files = container.files
    input.dispatchEvent(new Event('change'))
  })()
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
    const textArray = textContent.items.map(function (s) { return s.str })

    if (window.location.hash.includes("debug")) {
      console.group("text array from pdf.js")
      console.log(textArray)
      console.groupEnd()
    }

    return textArray
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

// Debugging utilities.
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min) + min); //The maximum is exclusive and the minimum is inclusive
}

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

