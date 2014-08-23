Parse.Cloud.define("publish", function(request, response) {
    var saveId = request.params.saveId;
    var levels = request.params.levels;

    var query = new Parse.Query("AppState");
    var appState;
    query.get(saveId).then(function(as) {
        appState = as; // FIXME is this good practice?

        var publishId = appState.get("publishId");

        var publishQuery = new Parse.Query("PublishState");
        if (publishId) {
            return publishQuery.get(publishId);
        } else {
            return new Parse.Object("PublishState");
        }
    }).then(function(publishState) {
        // check if whether -- if this publishState already existed --
        // it's actually keyed to this save; if it's not, the user
        // shouldn't be able to update it
        var publishSaveId = publishState.get("saveId");

        if (!publishSaveId || publishSaveId === saveId) {
            return Parse.Promise.as(publishState);
        } else {
            return Parse.Promise.error("Not authorized to publish here.");
        }
    }).then(function(publishState) {
        publishState.set("saveId", saveId);
        publishState.set("levels", levels);

        return publishState.save(null, { useMasterKey: true });

    }).then(function(publishState) {
        appState.set("publishId", publishState.id);
        appState.set("levels", levels);

        return appState.save();

    }).then(function(appState) {
        response.success({ publishId: appState.get("publishId") });

    }, function(error) {
        response.error(error);
    });
});

Parse.Cloud.define("getPublish", function(request, response) {
    var publishId = request.params.publishId;

    var query = new Parse.Query("PublishState");
    query.get(publishId, {
        useMasterKey: true,

        success: function(publishState) {
            response.success({ levels: publishState.get("levels") });
        },
        error: function(error) {
            response.error(error);
        }
    });
});
