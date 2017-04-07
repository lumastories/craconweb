"use strict";
function sub(f) {
  try {
    var r = new XMLHttpRequest();
    r.onload = function() {
      console.log(r.responseText);
    }
    r.open("POST", "http://localhost:8668/upload/ugimgset", true);
    r.setRequestHeader("Authorization", "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhdWQiOiJjcmFjb25hcGkiLCJleHAiOjE0OTE1NDQ0NTQsImlhdCI6MTQ5MTUxNTY1NCwiaXNzIjoiY3JhY29uYXBpIiwic3ViIjoiNzc3ODY3NDM4MDk0OTE4OTAwNyIsInJvbGVzIjpbeyJpZCI6Ijc2OTYyMzc3NjA5NDkxNTkwMTIiLCJuYW1lIjoiYWRtaW4iLCJ3ZWlnaHQiOjEwMDAwfV19.2IsyVLlHDFr2mCNFLfVHvlnNx0rYViBc1-iJWG8rTwE")

    console.log(document.getElementById('upload').files[0]);

    var fd = new FormData(f);
    r.send(fd);

  } catch(e) {
    console.log(e.message);
  }

  return false;
}

var flags = {"token": token()
            ,"time": Date.now()
            }

var app = Elm.Main.fullscreen(flags)

app.ports.fileSelected.subscribe(function (id){

    var node = document.getElementById(id)
    if (node === null){
        return
    }
    var file = node.files[0]
    var reader = new FileReader()
    reader.onload = (function(event){

        var csvFile = event.target.result
        var portData = {
            upload: csvFile,
            userid: node.className
        }

        // send file to elm port
        app.ports.fileContentRead.send(portData)
    })

    reader.readAsText(file)

})

app.ports.uploadFile.subscribe(function(userid){
    var form = document.getElementById("csvForm")
    var fd = new FormData(form);
    var xhr = new XMLHttpRequest()
    xhr.open('post', "http://localhost:8668/upload/ugimgset", true)
    xhr.setRequestHeader("Authorization", "Bearer " + token() )
    xhr.send(fd)
})

// Audio ports

app.ports.playAudioPing.subscribe(function(nothing) {
    document.getElementById("ping").play()
})

// Local Storage Ports

app.ports.storageGetItem.subscribe(function(key){
    app.ports.getUserResponse.send(localStorage.getItem(key) ? localStorage.getItem(key) : "")
})

app.ports.storageSetItem.subscribe(function(keyValue){
    var [key, value] = keyValue
    setLocalStorageItem(key, value)
})

app.ports.storageRemoveItem.subscribe(function(key){
    localStorage.removeItem(key)
})

app.ports.storageClear.subscribe(function(i){
    localStorage.clear()
})

/**
 * Set a value of any type in localStorage.
 * (Serializes in JSON before storing since Storage objects can only hold strings.)
 *
 * @param {String} key   Key in localStorage
 * @param {*}      value The value to set
 */
function setLocalStorageItem(key, value) {
  window.localStorage.setItem(key,
    JSON.stringify(value)
  )
}


function token(){
    if( localStorage.getItem("token") == null ){
        return "";
    }

    return JSON.parse(localStorage.getItem("token")).token;
}