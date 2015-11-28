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

    RuntimeInitialize() {
        this.App.Size.X = this.Canvas.width;
        this.App.Size.Y = this.Canvas.height;
        this.Graphics = new TGraphics(this.Canvas);

        this.Canvas.onmousedown = (ev: MouseEvent) => {
            console.log("onmousedown:" + ev.x + " " + ev.y);
        }

        this.Canvas.onmouseenter = (ev: MouseEvent) => {
            console.log("onmouseenter");
        }

        this.Canvas.onmouseleave = (ev: MouseEvent) => {
            console.log("onmouseleave");
        }

        this.Canvas.onmousemove = (ev: MouseEvent) => {
            console.log("onmousemove:" + ev.x + " " + ev.y);
        }

        this.Canvas.onmouseout = (ev: MouseEvent) => {
            console.log("onmouseout");
        }

        this.Canvas.onmouseover = (ev: MouseEvent) => {
            console.log("onmouseover");
        }

        this.Canvas.onmouseup = (ev: MouseEvent) => {
            console.log("onmouseup:" + ev.x + " " + ev.y);
        }

        this.Canvas.onmousewheel = (ev: MouseWheelEvent) => {
            console.log("onmousewheel");
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
                this.NaviShape( this.App.ShapeList[i] );
            }

            this.Graphics.Context.setTransform(1, 0, 0, 1, 0, 0);
            for (var i = 0; i < this.App.ShapeList.length; i++) {
                this.App.ShapeList[i].Draw(this.Graphics.Context);
            }
        }
        window.requestAnimationFrame(() => this.AnimationFrameLoop());
    }

    Run() {
        this.AnimationFrameLoop();
    }
}
