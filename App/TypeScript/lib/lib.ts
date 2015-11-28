class TPoint {
    X: number;
    Y: number;
}

class TRect {
    Position: TPoint = new TPoint();
    Size: TPoint = new TPoint();
}

class TShape {
    Parent: TShape = null;
    Position: TPoint = new TPoint();
    Size: TPoint = new TPoint();
    Rotation: number = 0;
    Velocity: TPoint = new TPoint();

    SetBoundingRectangle(x: number, y: number, w: number, h: number) {
        this.Position.X = x;
        this.Position.Y = y;
        this.Size.X = w;
        this.Size.Y = h;
    }

    Draw(ctx: CanvasRenderingContext2D) {
    }
}

class TGroup extends TShape {
    Children: TShape[] = [];

    Draw(ctx: CanvasRenderingContext2D) {
        ctx.save();

        var dx = this.Size.X / 2;
        var dy = this.Size.Y / 2;

        ctx.translate(this.Position.X + dx, this.Position.Y + dy);

        ctx.rotate(this.Rotation);

        for (var i: number = 0; i < this.Children.length; i++) {
            this.Children[i].Draw(ctx);
        }

        ctx.restore();

    }
}

class TPolygon extends TShape {
    Draw(ctx: CanvasRenderingContext2D) {

        /* 四角を描く */
        ctx.beginPath();
        ctx.moveTo(this.Position.X, this.Position.Y);
        ctx.lineTo(this.Position.X + this.Size.X, this.Position.Y);
        ctx.lineTo(this.Position.X + this.Size.X, this.Position.Y + this.Size.Y);
        ctx.lineTo(this.Position.X, this.Position.Y + this.Size.Y);
        ctx.closePath();
        ctx.stroke();
    }
}

class TRectangle extends TShape {
    BackgroundColor: string = null;
    BorderWidth: number = 0;
    BorderColor: string = null;

    Draw(ctx: CanvasRenderingContext2D) {
        ctx.save();

        ctx.beginPath();

        var dx = this.Size.X / 2;
        var dy = this.Size.Y / 2;

        ctx.translate(this.Position.X + dx, this.Position.Y + dy);

        ctx.rotate(this.Rotation);

        if (this.BackgroundColor != null) {
            ctx.fillStyle = this.BackgroundColor;
            ctx.fillRect(-dx, -dy, this.Size.X, this.Size.Y);
        }

        if (this.BorderWidth != 0 && this.BorderColor != null) {
            ctx.strokeStyle = this.BorderColor;
            ctx.strokeRect(-dx, -dy, this.Size.X, this.Size.Y);
        }

        ctx.closePath();
        ctx.restore();
    }
}

class TLabel extends TShape {
    BackgroundColor: string = null;
    BorderWidth: number = 0;
    BorderColor: string = null;
    TextColor: string = "#000000";
    Text: string = "";

    Draw(ctx: CanvasRenderingContext2D) {
        ctx.save();
        ctx.beginPath();

        var dx = this.Size.X / 2;
        var dy = this.Size.Y / 2;

        ctx.translate(this.Position.X + dx, this.Position.Y + dx);

        ctx.rotate(this.Rotation);

        ctx.textBaseline = "top";
        ctx.font = "40px 'ＭＳ Ｐゴシック'";
        if (this.BackgroundColor != null) {
            ctx.fillStyle = this.BackgroundColor;
            ctx.fillRect(-dx, -dy, this.Size.X, this.Size.Y);
        }

        if (this.BorderWidth != 0 && this.BorderColor != null) {
            ctx.strokeStyle = this.BorderColor;
            ctx.strokeRect(-dx, -dy, this.Size.X, this.Size.Y);
        }

        ctx.fillStyle = this.TextColor;
        ctx.fillText(this.Text, -dx, -dy, 80);

        ctx.closePath();
        ctx.restore();
    }
}

class TEllipse extends TShape {
    BackgroundColor: string = null;
    BorderWidth: number = 0;
    BorderColor: string = null;

    Draw(ctx: CanvasRenderingContext2D) {
        ctx.save();
        ctx.beginPath();

        var cx: number = this.Position.X + this.Size.X / 2;
        var cy: number = this.Position.Y + this.Size.Y / 2;
        var rx: number = this.Size.X / 2;

        if (this.BackgroundColor != null) {
            ctx.fillStyle = this.BackgroundColor;
        }
        if (this.BorderWidth != 0 && this.BorderColor != null) {
            ctx.strokeStyle = this.BorderColor;
        }

        ctx.translate(cx, cy);

        ctx.rotate(this.Rotation);
        ctx.scale(1, this.Size.Y / this.Size.X);
        ctx.arc(0, 0, rx, 0, Math.PI * 2, false);

        if (this.BackgroundColor != null) {
            ctx.fill();
        }

        if (this.BorderWidth != 0 && this.BorderColor != null) {
            ctx.stroke();
        }

        ctx.closePath();
        ctx.restore();
    }
}

class TImage extends TShape {
    Loaded: boolean = false;
    ImageIm: HTMLImageElement;

    Load(src_url: string) {
        this.ImageIm = new Image();
        this.ImageIm.src = src_url;
    }

    Draw(ctx: CanvasRenderingContext2D) {
        if (this.ImageIm.width == undefined || this.ImageIm.width == null) {
            return;
        }
        ctx.save();
        ctx.beginPath();

        var rx = this.Size.X  / this.ImageIm.width;
        var ry = this.Size.Y / this.ImageIm.height;

        var dx = this.Size.X / 2;
        var dy = this.Size.Y / 2;

        ctx.translate(this.Position.X + dx, this.Position.Y + dy);

        ctx.scale(rx, ry);
        ctx.rotate(this.Rotation);

        ctx.drawImage(this.ImageIm, -dx / rx, -dy / ry);

        ctx.closePath();
        ctx.restore();
    }    
}

class TGraphics {
    Context: CanvasRenderingContext2D;
    Canvas: HTMLCanvasElement;

    constructor(canvas : HTMLCanvasElement) {
        this.Canvas = canvas;
        this.Context = this.Canvas.getContext("2d");
    }

    draw() {
        var rct = new TPolygon();
        rct.SetBoundingRectangle(20, 20, 100, 50);
        rct.Draw(this.Context);
    }

    /* clearRect()の例 */
    draw3() {
        this.Context.beginPath();
        this.Context.fillRect(20, 20, 100, 100);
        this.Context.beginPath();
        this.Context.clearRect(50, 70, 60, 30);
    }

    draw4() {
        /* 円 #1 */
        this.Context.beginPath();
        this.Context.fillStyle = "rgb(192, 80, 77)"; // 赤
        this.Context.arc(70, 445, 35, 0, Math.PI * 2, false);
        this.Context.fill();
        /* 円 #2 */
        this.Context.beginPath();
        this.Context.fillStyle = "rgb(155, 187, 89)"; // 緑
        this.Context.arc(45, 495, 35, 0, Math.PI * 2, false);
        this.Context.fill();
        /* 円 #3 */
        this.Context.beginPath();
        this.Context.fillStyle = "rgb(128, 100, 162)"; // 紫
        this.Context.arc(95, 495, 35, 0, Math.PI * 2, false);
        this.Context.fill();
    }

    draw5() {
        this.Context.beginPath();

        /* フォントスタイルを定義 */
        this.Context.font = "48px 'ＭＳ Ｐゴシック'";
        /* 青色でstrokText */
        this.Context.strokeStyle = "blue";
        this.Context.strokeText("青色でstrokText", 10, 525, 80);
        /* maxLengthをセット */
        this.Context.strokeText("maxLengthをセット", 10, 550, 80);
        /* 赤色でfillText */
        this.Context.fillStyle = "red";
        this.Context.fillText("赤色でfillText", 10, 75, 580);
        /* maxLengthをセット */
        this.Context.fillText("maxLengthをセット", 10, 600, 80);
    }

    Clear() {
        this.Context.clearRect(0, 0, this.Canvas.width, this.Canvas.height);
    }

    Animation() {
        this.draw();

//        this.draw3();
        this.draw4();
        this.draw5();

    }
}

class TApplication {
    Size: TPoint = new TPoint();
    ShapeList: TShape[];

    AppInitialize() {
    }

    Rule(self: Object) {
    }
}
