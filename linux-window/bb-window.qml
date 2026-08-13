import QtQuick
import QtWebEngine

Window {
    id: root
    visible: true
    width: 1440
    height: 960
    title: "bb"

    function isBbUrl(candidate) {
        return candidate.toString().startsWith("http://127.0.0.1:38886")
    }

    WebEngineView {
        anchors.fill: parent
        url: "http://127.0.0.1:38886"

        onNavigationRequested: function(request) {
            if (request.isMainFrame && !root.isBbUrl(request.url)) {
                request.reject()
                Qt.openUrlExternally(request.url)
            }
        }

        onNewWindowRequested: function(request) {
            Qt.openUrlExternally(request.requestedUrl)
        }
    }
}
