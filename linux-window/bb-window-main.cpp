#include <QGuiApplication>
#include <QQmlApplicationEngine>

int main(int argc, char *argv[]) {
    QGuiApplication application(argc, argv);
    application.setApplicationName("bb-window");
    application.setDesktopFileName("bb-window");

    QQmlApplicationEngine engine;
    engine.load(QUrl::fromLocalFile("/home/naftoli/.local/share/bb-window/bb-window.qml"));
    if (engine.rootObjects().isEmpty())
        return 1;

    return application.exec();
}
