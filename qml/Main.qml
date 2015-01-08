import QtQuick 2.0

Item {
    property double n : 0;
    NumberAnimation on n {
        from: 0; to: 3.1;
        duration: 15000;
        onStopped: finished();
    }

    Row {
        BasicTile {
           startAngle: 0;
           endAngle: 90;
           volume: Math.max(Math.min(n-0, 1.1), 0);
        }
        BasicTile {
           startAngle: 270;
           endAngle: 90;
           volume: Math.max(Math.min(n-1, 1.1), 0);
        }
        BasicTile {
           startAngle: 270;
           endAngle: 180;
           volume: Math.max(Math.min(n-2, 1.1), 0);
        }
    }
}