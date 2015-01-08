import QtQuick 2.0
import QtGraphicalEffects 1.0

Item {
    id: c;
    width: 100;
    height: 100;

    property int startAngle : -1;
    property int endAngle : -1;
    property double volume : 0.5;

    function rotate(a, d) {
        return isOrtho(a) ? (a+d)%360 : undefined;
    }

    function isOrtho(a) {
        return a == 0 || a == 90 || a == 180 || a == 270;
    }

    LinearGradient {
        function steer(n, dir) {
            switch (dir) {
            case 0: return {x: 0.5, y: n};
            case 90: return {x: 1-n, y: 0.5};
            case 180: return {x: 0.5, y: 1-n};
            case 270: return {x: n, y: 0.5};
            }
            return {x: 0.5, y: 0.5};
        }

        function point(p) {
            return Qt.point(p.x*c.width, p.y*c.height);
        }

        function calc(vol, t) {
            var band = 0.3;
            var t2 = t + band;
            var startAngle = c.startAngle;
            var endAngle = rotate(c.endAngle, 180);
            if (!isOrtho(c.startAngle)) {
                vol = 0.5+vol/2;
            }
            else if (!isOrtho(c.endAngle)) {
                vol = vol/2;
            }
            if (vol < t) {
                return point(steer(vol, startAngle));
            }
            else if (vol < t2) {
                var v = (vol-t)/band;
                var a = steer(t, startAngle);
                var b = steer(t2, endAngle);
                return point({x: a.x*(1-v)+b.x*v, y: a.y*(1-v)+b.y*v});
            }
            else {
                return point(steer(vol, endAngle));
            }
        }

        anchors.fill: parent;
        start: calc(c.volume-0.1, 0.3);
        end: calc(c.volume, 0.4);
        gradient: Gradient {
            GradientStop { position: 0.0; color: "blue"; }
            GradientStop { position: 1.0; color: "white"; }
        }
    }
    Image {
        id: straightMask;
        anchors.fill: parent;
        visible: rotate(c.startAngle, 180) == c.endAngle;
        rotation: c.startAngle;
        source: 'straight_mask.svg';
    }
    Image {
        id: cornerMask;
        anchors.fill: parent;
        visible:
            rotate(c.startAngle, 90) == c.endAngle ||
            rotate(c.endAngle, 90) == c.startAngle;
        rotation: rotate(c.startAngle, 90) == c.endAngle ?
            c.startAngle : c.endAngle;
        source: 'corner_mask.svg';
    }
    Image {
        id: endMask;
        anchors.fill: parent;
        visible: isOrtho(c.startAngle) ^ isOrtho(c.endAngle);
        rotation: isOrtho(c.startAngle) ? c.startAngle : c.endAngle;
        source: 'end_mask.svg';
    }
    Text {
        anchors.centerIn: parent;
        visible: endMask.visible;
        font.pixelSize: 20; color: 'white';
        text: isOrtho(c.startAngle) ? 'E' : 'S';
    }
    Image {
        anchors.fill: parent;
        source: 'full_mask.svg';
        visible:
            !straightMask.visible && !cornerMask.visible && !endMask.visible;
    }
}
