var flags = {"token": token()
            ,"time": Date.now()
            }

var app = Elm.Main.fullscreen(flags)

app.ports.uploadFile.subscribe(function(pathAndId){
    var [tasksrvPath, formId] = pathAndId
    try {
        var fd = new FormData(document.getElementById(formId));
        var r = new XMLHttpRequest()
        r.open("POST", tasksrvPath, true)
        r.setRequestHeader("Authorization", "Bearer " + token() )
        r.send(fd)
        r.onload = function() {
            app.ports.status.send(r.statusText)
        }
    } catch(e) {
        console.log(e.message);
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


function token(){
    if( localStorage.getItem("token") == null ){
        return "";
    }

    return JSON.parse(localStorage.getItem("token")).token;
}