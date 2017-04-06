function token(){
    if(localStorage.getItem("token")==null){
      return "";
    } else {
      return JSON.parse(localStorage.getItem("token")).token;
    }
}


var flags = {"token": token()
            , "time": Date.now()
            }

console.log(flags);

var app = Elm.Main.fullscreen(flags);


// File reader ports

app.ports.fileSelected.subscribe(function (id){

    var node = document.getElementById(id);
    if (node === null){
        return;
    }
    var file = node.files[0];
    var reader = new FileReader();
    reader.onload = (function(event){
        // target is the file
        var csvFile = event.target.result;
        var portData = {
            upload: csvFile,
            userid: node.className
        };

        // send file to elm port
        app.ports.fileContentRead.send(portData);
    });

    reader.readAsText(file);

});

app.ports.uploadFile.subscribe(function(userid){

    function filePicker(){
        return document.getElementById("csv");
    }

    var file = filePicker().files[0];
    var formData = new FormData();

    formData.append("upload", file);
    formData.append("userid", userid);

    var xhr = new XMLHttpRequest();

    xhr.onreadystatechange = function(e) {
        if ( 4 == this.readyState ) {
            console.log(['xhr upload complete', e]);
        }
    };

    xhr.open('post', "http://localhost:8668/upload/ugimgset", true);
    xhr.setRequestHeader("Content-Type","multipart/form-data");
    xhr.setRequestHeader("Authorization", "Bearer " + token() );
    xhr.send(formData);
});

// Audio ports

app.ports.playAudioPing.subscribe(function(nothing) {
    document.getElementById("ping").play();
});

// Local Storage Ports

app.ports.storageGetItem.subscribe(function(key){
    app.ports.getUserResponse.send(localStorage.getItem(key) ? localStorage.getItem(key) : "");
});

app.ports.storageSetItem.subscribe(function(keyValue){
    var [key, value] = keyValue;
    setLocalStorageItem(key, value);
});

app.ports.storageRemoveItem.subscribe(function(key){
    localStorage.removeItem(key);
});

app.ports.storageClear.subscribe(function(i){
    localStorage.clear();
});

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
  );
}
