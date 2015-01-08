import QtQuick 2.0

Item {
    property double n : 0;
    NumberAnimation on n {
        from: 0; to: plumb.length + 0.1;
        duration: 5000 * (plumb.length + 0.1);
        onStopped: finished();
    }

    Repeater {
        model: plumb;

        BasicTile {
            x: modelData.x*100;
            y: modelData.y*100;
            startAngle: modelData.enterAngle;
            endAngle: modelData.exitAngle;
            volume: Math.max(Math.min(n-index, 1.1), 0);
        }
    }
}
