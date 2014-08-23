
// Use Parse.Cloud.define to define as many cloud functions as you want.
// For example:
Parse.Cloud.define("publish", function(request, response) {
    var saveId = request.params.saveId;

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
        publishState.set("levels", appState.get("levels"));

        return publishState.save();

    }).then(function(publishState) {
        appState.set("publishId", publishState.id);

        return appState.save();

    }).then(function(appState) {
        response.success({ publishId: appState.get("publishId") });

    }, function(error) {
        response.error(error);
    });
});
