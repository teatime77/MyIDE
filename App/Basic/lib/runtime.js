/// <reference path="../lib/lib.ts"/>
window.onload = function () {
    //var requestAnimationFrame = window.requestAnimationFrame || window.mozRequestAnimationFrame ||
    //    window.webkitRequestAnimationFrame || window.msRequestAnimationFrame;
    //window.requestAnimationFrame = requestAnimationFrame;
    var canvas = document.getElementById("canvassample");
    /* canvas要素の存在チェックとCanvas未対応ブラウザの対処 */
    if (canvas == null) {
        return false;
    }
    //if (!canvas.getContext) {
    //    return false;
    //}
    var runtime = new TRuntime();
    runtime.App = new TMyApplication();
    var canvas;
    runtime.Canvas = document.getElementById("canvassample");
    runtime.RuntimeInitialize();
};
var TRuntime = (function () {
    function TRuntime() {
    }
    TRuntime.prototype.SetMouseEvent = function (ev) {
        console.log("SetMouseEvent");
        var rc = this.Canvas.getBoundingClientRect();
        var x = (window.pageXOffset !== undefined) ? window.pageXOffset : (document.documentElement || document.body.parentNode || document.body)["scrollLeft"];
        var y = (window.pageYOffset !== undefined) ? window.pageYOffset : (document.documentElement || document.body.parentNode || document.body)["scrollTop"];
        console.log("SetMouseEvent:" + rc.left + " " + window.pageXOffset + " " + document.documentElement.scrollLeft);
        this.App.MousePosition.X = ev.clientX - rc.left; // ev.x - (x + rc.left);
        this.App.MousePosition.Y = ev.clientY - rc.top; // ev.y - (y + rc.top);
    };
    TRuntime.prototype.RuntimeInitialize = function () {
        var _this = this;
        this.App.Size.X = this.Canvas.width;
        this.App.Size.Y = this.Canvas.height;
        this.Graphics = new TGraphics(this.Canvas);
        this.Canvas.onmousedown = function (ev) {
            _this.SetMouseEvent(ev);
            console.log("onmousedown:" + ev.clientX + " " + ev.clientY);
        };
        this.Canvas.onmouseenter = function (ev) {
            //this.SetMouseEvent(ev);
            //            console.log("onmouseenter");
        };
        this.Canvas.onmouseleave = function (ev) {
            //this.SetMouseEvent(ev);
            //console.log("onmouseleave");
        };
        this.Canvas.onmousemove = function (ev) {
            _this.SetMouseEvent(ev);
            //console.log("onmousemove:" + ev.x + " " + ev.y);
        };
        this.Canvas.onmouseout = function (ev) {
            //this.SetMouseEvent(ev);
            //console.log("onmouseout");
        };
        this.Canvas.onmouseover = function (ev) {
            //this.SetMouseEvent(ev);
            //console.log("onmouseover");
        };
        this.Canvas.onmouseup = function (ev) {
            //this.SetMouseEvent(ev);
            //console.log("onmouseup:" + ev.x + " " + ev.y);
        };
        this.Canvas.onmousewheel = function (ev) {
            //this.SetMouseEvent(ev);
            //console.log("onmousewheel");
        };
        this.App.AppInitialize();
        this.Run();
    };
    TRuntime.prototype.NaviShape = function (self) {
        this.App.Rule(self, this.App);
        if (self instanceof TGroup) {
            for (var i = 0; i < self.Children.length; i++) {
                this.NaviShape(self.Children[i]);
            }
        }
    };
    TRuntime.prototype.AnimationFrameLoop = function () {
        var _this = this;
        this.Graphics.Clear();
        if (this.App.ShapeList != null) {
            for (var i = 0; i < this.App.ShapeList.length; i++) {
                this.NaviShape(this.App.ShapeList[i]);
            }
            this.Graphics.Context.setTransform(1, 0, 0, 1, 0, 0);
            for (var i = 0; i < this.App.ShapeList.length; i++) {
                this.App.ShapeList[i].Draw(this.Graphics);
            }
        }
        window.requestAnimationFrame(function () { return _this.AnimationFrameLoop(); });
    };
    TRuntime.prototype.Run = function () {
        this.AnimationFrameLoop();
    };
    return TRuntime;
})();

function Inherits(sub_class, super_class) {
    sub_class.SuperClass = super_class;

    for (var p in super_class) {
        if (super_class.hasOwnProperty(p)) {
            sub_class[p] = super_class[p];
        }
    }

    function __() {
        this.constructor = sub_class;
    }

    __.prototype = super_class.prototype;

    sub_class.prototype = new __();
};
//# sourceMappingURL=runtime.js.map