function addMp3Elem(id, srcPath) {
    var a = document.createElement('audio');
    a.id = id;
    a.type = "audio/mpeg";

    var srcPrefix = "https://file.cravecontrol.org";
    if (window.location.host == "localhost") {
        srcPrefix = "localhost:8654/";
    }
    a.src = srcPrefix + srcPath;

    if (document.body != null) {
        document.body.appendChild(a);
    }
}

addMp3Elem("ping", "/repo/ding.mp3");

var flags = {
    "token": token(),
    "time": Date.now()
}

var app = Elm.Main.fullscreen(flags)

// Ports


document.onreadystatechange = function () {
  if (document.readyState === "complete") {
    app.ports.domLoaded.send(true);
  }
}

app.ports.set.subscribe(function(keyValue) {
    var [key, value] = keyValue
    setLocalStorageItem(key, value)
})

app.ports.remove.subscribe(function(key) {
    localStorage.removeItem(key)
})

app.ports.clear.subscribe(function(i) {
    localStorage.clear()
})

app.ports.ping.subscribe(function(nothing) {
    document.getElementById("ping").play()
})

app.ports.preload.subscribe(function(urls) {
        urlsLen = urls.length;
        loadedImages = new Array()
        for (i = 0; i < urlsLen; i++) {
            loadedImages[i] = new Image();
            loadedImages[i].src = urls[i];
        };
    })
    /**
     * in: where to upload, css form id, token to use
     * side: Uploads form as multi-part, sends statusText to status port
     * out: nothing
     */

app.ports.upload.subscribe(function(pathIdToken) {
    var [tasksrvPath, formId, token] = pathIdToken
    try {
        var fd = new FormData(document.getElementById(formId));
        var r = new XMLHttpRequest()
        r.open("POST", tasksrvPath, true)
        r.setRequestHeader("Authorization", "Bearer " + token)
        r.send(fd)
        r.onload = function() {
            app.ports.status.send(r.response)
        }
    } catch (e) {
        console.log(e.message);
    }

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

/**
 * Get a token or an empty string if token does not exist
 * (Serializes in JSON before storing since Storage objects can only hold strings.)
 *
 * @param {}
 * @returns    jwt token as a string
 */
function token() {
    if (localStorage.getItem("token") == null) {
        return "";
    }
    try {
        return JSON.parse(localStorage.getItem("token")).token;
    } catch (e) {
        return ""
    }
}