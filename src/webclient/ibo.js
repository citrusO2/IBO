"use strict";

(function(){
    var username = "tu", password = "pass123";

    $.ajax({
        type: "GET",
        url: "/api/box/overview",
        dataType: 'text',
        headers: {
            "Authorization": "Basic " + window.btoa(username + ":" + password)
        }
    }).done(function(msg){
        alert("Server: " + msg);
    }).fail(function(jqXHR, textStatus){
        alert("Request failed: " + textStatus);
    });

})();