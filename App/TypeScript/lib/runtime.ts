/// <reference path="../lib/lib.ts"/>

window.onload = () => {
    //var requestAnimationFrame = window.requestAnimationFrame || window.mozRequestAnimationFrame ||
    //    window.webkitRequestAnimationFrame || window.msRequestAnimationFrame;
    //window.requestAnimationFrame = requestAnimationFrame;

    var canvas: HTMLCanvasElement = <HTMLCanvasElement>document.getElementById("canvassample");
    /* canvas要素の存在チェックとCanvas未対応ブラウザの対処 */
    if (canvas == null) {
        return false;
    }
    //if (!canvas.getContext) {
    //    return false;
    //}

    var runtime: TRuntime = new TRuntime();

    runtime.App = new TMyApplication();
    var canvas: HTMLCanvasElement;

    runtime.Canvas = <HTMLCanvasElement> document.getElementById("canvassample");
    runtime.RuntimeInitialize();
};

class TRuntime {
    App: TApplication;
    Canvas: HTMLCanvasElement;
    Graphics: TGraphics;

    SetMouseEvent(ev: MouseEvent) {
        console.log("SetMouseEvent");
        var rc: ClientRect = this.Canvas.getBoundingClientRect();
        var x : number = (window.pageXOffset !== undefined) ? window.pageXOffset : (document.documentElement || document.body.parentNode || document.body)["scrollLeft"];
        var y : number = (window.pageYOffset !== undefined) ? window.pageYOffset : (document.documentElement || document.body.parentNode || document.body)["scrollTop"];
        console.log("SetMouseEvent:" + rc.left + " " + window.pageXOffset + " " + document.documentElement.scrollLeft);

        this.App.MousePosition.X = ev.clientX - rc.left;// ev.x - (x + rc.left);
        this.App.MousePosition.Y = ev.clientY - rc.top;// ev.y - (y + rc.top);
    }

    RuntimeInitialize() {
        this.App.Size.X = this.Canvas.width;
        this.App.Size.Y = this.Canvas.height;
        this.Graphics = new TGraphics(this.Canvas);

        this.Canvas.onmousedown = (ev: MouseEvent) => {
            this.SetMouseEvent(ev);
            console.log("onmousedown:" + ev.clientX + " " + ev.clientY);
        }

        this.Canvas.onmouseenter = (ev: MouseEvent) => {
            //this.SetMouseEvent(ev);
//            console.log("onmouseenter");
        }

        this.Canvas.onmouseleave = (ev: MouseEvent) => {
            //this.SetMouseEvent(ev);
            //console.log("onmouseleave");
        }

        this.Canvas.onmousemove = (ev: MouseEvent) => {
            this.SetMouseEvent(ev);
            //console.log("onmousemove:" + ev.x + " " + ev.y);
        }

        this.Canvas.onmouseout = (ev: MouseEvent) => {
            //this.SetMouseEvent(ev);
            //console.log("onmouseout");
        }

        this.Canvas.onmouseover = (ev: MouseEvent) => {
            //this.SetMouseEvent(ev);
            //console.log("onmouseover");
        }

        this.Canvas.onmouseup = (ev: MouseEvent) => {
            //this.SetMouseEvent(ev);
            //console.log("onmouseup:" + ev.x + " " + ev.y);
        }

        this.Canvas.onmousewheel = (ev: MouseWheelEvent) => {
            //this.SetMouseEvent(ev);
            //console.log("onmousewheel");
        }

        this.App.AppInitialize();
        this.Run();
    }

    NaviShape(self: Object) {
        this.App.Rule(self);
        if (self instanceof TGroup) {
            for (var i: number = 0; i < self.Children.length; i++) {
                this.NaviShape(self.Children[i]);
            }
        }
    }

    AnimationFrameLoop() {
        this.Graphics.Clear();

        if (this.App.ShapeList != null) {
            for (var i = 0; i < this.App.ShapeList.length; i++) {
                this.NaviShape(this.App.ShapeList[i] );
            }

            this.Graphics.Context.setTransform(1, 0, 0, 1, 0, 0);
            for (var i = 0; i < this.App.ShapeList.length; i++) {
                this.App.ShapeList[i].Draw(this.Graphics);
            }
        }
        window.requestAnimationFrame(() => this.AnimationFrameLoop());
    }

    Run() {
        this.AnimationFrameLoop();
    }
}
