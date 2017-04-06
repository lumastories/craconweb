function token(){
    if(localStorage.getItem("token")==null){
      return ""
    } else {
      return JSON.parse(localStorage.getItem("token")).token
    }
}


var flags = {"token": token()
            , "time": Date.now()
            }

console.log(flags)

var app = Elm.Main.fullscreen(flags)


// File reader ports

app.ports.fileSelected.subscribe(function (id){

    var node = document.getElementById(id)
    if (node === null){
        return
    }
    var file = node.files[0]
    var reader = new FileReader()
    reader.onload = (function(event){
        // target is the file
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
    var file = document.getElementById("csv").files[0];
    var fr = new FileReader();
    fr.readAsText(file, "UTF-8");
    fr.onload = function (evt) {
        console.log(evt.target.result);
        var fd = new FormData();
        fd.append("userid", userid)
        fd.append("upload", file) // <<<<< when I change this to evt.target.result I get
        var xhr = new XMLHttpRequest()
        xhr.open('post', "http://localhost:8668/upload/ugimgset", true)
        xhr.setRequestHeader("Content-Type", "multipart/form-data")
        xhr.setRequestHeader("Authorization", "Bearer " + token() )
        xhr.send(fd)
    }
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
