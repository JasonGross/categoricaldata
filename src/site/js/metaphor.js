function storeOntologyIfLargerThan(ontology, limit, callback) {
    if(encodeURI(ontology).length > limit) {
        $.ajax({
        beforeSend: function(req) {
                req.setRequestHeader("Accept", "application/json");
            },
        type:"POST",
        url: "/metaphor/store/ontology",
		data: { "ontology": $("#ontology-ajax").val() },
        success: function(data,status,jqXHR){
        	callback(jqXHR.getResponseHeader('Location'));
        }
        });
    } else {
        callback(ontology);
    }
}

function storeOntologyIfTooLarge(ontology, callback) = {
    storeOntologyIfLargerThan(ontology, 1000, callback);
}