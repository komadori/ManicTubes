import QtQuick 2.0

Grid {
    columns: 5;
    columnSpacing: 5;
    rowSpacing: 5;

    property double vol: 0;
    NumberAnimation on vol {
        from: 0; to: 1.1; duration: 5000;
        loops: Animation.Infinite;
    }

    Repeater {
        model: 25;

        BasicTile {
            startAngle: Math.floor(index / 5) * 90;
            endAngle: (index % 5) * 90;
            volume: vol;
        }
    }
}
