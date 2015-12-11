class TPoint {
    X: number;
    Y: number;

    Distance(p: TPoint): number {
        var dx = p.X - this.X;
        var dy = p.Y - this.Y;

        return Math.sqrt(dx * dx + dy * dy);
    }
}

class TMat2D {
    m11: number=1;
    m12: number=0;
    m21: number=0;
    m22: number=1;
    dx: number=0;
    dy: number = 0;

    Copy() {
        var A: TMat2D = new TMat2D();

        A.m11 = this.m11;
        A.m12 = this.m12;
        A.m21 = this.m21;
        A.m22 = this.m22;
        A.dx = this.dx;
        A.dy = this.dy;

        return A;
    }

    Mul(A: TMat2D) {
        var B: TMat2D = new TMat2D();

        B.m11 = this.m11 * A.m11 + this.m21 * A.m12;
        B.m21 = this.m11 * A.m21 + this.m21 * A.m22;
        B.dx = this.m11 * A.dx + this.m21 * A.dy + this.dx;

        B.m12 = this.m12 * A.m11 + this.m22 * A.m12;
        B.m22 = this.m12 * A.m21 + this.m22 * A.m22;
        B.dy = this.m12 * A.dx + this.m22 * A.dy + this.dy;

        return B;
    }

    MulPoint(p1 : TPoint) {
        var p2: TPoint = new TPoint();

        p2.X = this.m11 * p1.X + this.m21 * p1.Y + this.dx;
        p2.Y = this.m12 * p1.X + this.m22 * p1.Y + this.dy;

        return p2;
    }

    transform(A: TMat2D) {
        var m11_: number, m12_: number, m21_: number, m22_: number, dx_: number, dy_: number;

        m11_ = this.m11 * A.m11 + this.m21 * A.m12;
        m21_ = this.m11 * A.m21 + this.m21 * A.m22;
        dx_ = this.m11 * A.dx + this.m21 * A.dy + this.dx;

        m12_ = this.m12 * A.m11 + this.m22 * A.m12;
        m22_ = this.m12 * A.m21 + this.m22 * A.m22;
        dy_ = this.m12 * A.dx + this.m22 * A.dy + this.dy;

        this.m11 = m11_;
        this.m12 = m12_;
        this.m21 = m21_;
        this.m22 = m22_;
        this.dx = dx_;
        this.dy = dy_;
    }

    scale(x:number,y:number) {
        this.m11 *= x;
        this.m21 *= y;

        this.m12 *= x;
        this.m22 *= y;
    }

    translate(x: number, y: number) {
        this.dx += this.m11 * x + this.m21 * y;
        this.dy += this.m12 * x + this.m22 * y;
    }


    rotate(r: number) {
        var m11_: number, m12_: number, m21_: number, m22_: number;
        var cos_r: number = Math.cos(r);
        var sin_r: number = Math.sin(r);

        m11_ = this.m11 * cos_r + this.m21 * sin_r;
        m21_ = this.m11 * -sin_r + this.m21 * cos_r;

        m12_ = this.m12 * cos_r + this.m22 * sin_r;
        m22_ = this.m12 * -sin_r + this.m22 * cos_r;

        this.m11 = m11_;
        this.m12 = m12_;
        this.m21 = m21_;
        this.m22 = m22_;
    }
}

class TRect {
    Position: TPoint = new TPoint();
    Size: TPoint = new TPoint();
}

function weak(x) {
    return x;
}

class TShape {
    @weak
    Parent: TShape = null;
    Center: TPoint = new TPoint();
    Radius: number;
    AbsCenter: TPoint = new TPoint();
    Size: TPoint = new TPoint();
    Rotation: number = 0;
    Velocity: TPoint = new TPoint();

    SetBoundingRectangle(x: number, y: number, w: number, h: number) {
        this.Center.X = x;
        this.Center.Y = y;
        this.Size.X = w;
        this.Size.Y = h;
        this.Radius = Math.max(w, h) / 2;
    }

    Draw(gr : TGraphics) {
    }
}

class TGroup extends TShape {
    Children: TShape[] = [];

    Draw(gr : TGraphics) {
        var ctx: CanvasRenderingContext2D = gr.Context;
        gr.save();

        this.AbsCenter = gr.Transform.MulPoint(this.Center);
        gr.translate(this.Center.X, this.Center.Y);

        gr.rotate(this.Rotation);

        for (var i: number = 0; i < this.Children.length; i++) {
            this.Children[i].Draw(gr);
        }

        gr.restore();

    }
}

class TPolygon extends TShape {
    Draw(gr : TGraphics) {
        var ctx: CanvasRenderingContext2D = gr.Context;

        /* 四角を描く */
        ctx.beginPath();
        ctx.moveTo(this.Center.X, this.Center.Y);
        ctx.lineTo(this.Center.X + this.Size.X, this.Center.Y);
        ctx.lineTo(this.Center.X + this.Size.X, this.Center.Y + this.Size.Y);
        ctx.lineTo(this.Center.X, this.Center.Y + this.Size.Y);
        ctx.closePath();
        ctx.stroke();
    }
}

class TRectangle extends TShape {
    BackgroundColor: string = null;
    BorderWidth: number = 0;
    BorderColor: string = null;

    Draw(gr : TGraphics) {
        var ctx: CanvasRenderingContext2D = gr.Context;
        gr.save();

        ctx.beginPath();

        var dx = this.Size.X / 2;
        var dy = this.Size.Y / 2;

        this.AbsCenter = gr.Transform.MulPoint(this.Center);
        gr.translate(this.Center.X, this.Center.Y);

        gr.rotate(this.Rotation);

        if (this.BackgroundColor != null) {
            ctx.fillStyle = this.BackgroundColor;
            ctx.fillRect(-dx, -dy, this.Size.X, this.Size.Y);
        }

        if (this.BorderWidth != 0 && this.BorderColor != null) {
            ctx.strokeStyle = this.BorderColor;
            ctx.strokeRect(-dx, -dy, this.Size.X, this.Size.Y);
        }

        ctx.closePath();
        gr.restore();
    }
}

class TLabel extends TShape {
    BackgroundColor: string = null;
    BorderWidth: number = 0;
    BorderColor: string = null;
    TextColor: string = "#000000";
    Text: string = "";

    Draw(gr : TGraphics) {
        var ctx: CanvasRenderingContext2D = gr.Context;
        gr.save();
        ctx.beginPath();

        var dx = this.Size.X / 2;
        var dy = this.Size.Y / 2;

        this.AbsCenter = gr.Transform.MulPoint(this.Center);
        gr.translate(this.Center.X, this.Center.Y);

        gr.rotate(this.Rotation);

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
        gr.restore();
    }
}

class TEllipse extends TShape {
    BackgroundColor: string = null;
    BorderWidth: number = 0;
    BorderColor: string = null;

    Draw(gr : TGraphics) {
        var ctx: CanvasRenderingContext2D = gr.Context;
        gr.save();
        ctx.beginPath();

        var rx: number = this.Size.X / 2;

        if (this.BackgroundColor != null) {
            ctx.fillStyle = this.BackgroundColor;
        }
        if (this.BorderWidth != 0 && this.BorderColor != null) {
            ctx.strokeStyle = this.BorderColor;
        }

        this.AbsCenter = gr.Transform.MulPoint(this.Center);
        gr.translate(this.Center.X, this.Center.Y);

        gr.rotate(this.Rotation);
        gr.scale(1, this.Size.Y / this.Size.X);
        ctx.arc(0, 0, rx, 0, Math.PI * 2, false);

        if (this.BackgroundColor != null) {
            ctx.fill();
        }

        if (this.BorderWidth != 0 && this.BorderColor != null) {
            ctx.stroke();
        }

        ctx.closePath();
        gr.restore();
    }
}

class TImage extends TShape {
    Loaded: boolean = false;
    ImageIm: HTMLImageElement;

    Load(src_url: string) {
        this.ImageIm = new Image();
        this.ImageIm.src = src_url;
    }

    Draw(gr : TGraphics) {
        var ctx: CanvasRenderingContext2D = gr.Context;
        if (this.ImageIm.width == undefined || this.ImageIm.width == null) {
            return;
        }
        gr.save();
        ctx.beginPath();

        var rx = this.Size.X  / this.ImageIm.width;
        var ry = this.Size.Y / this.ImageIm.height;

        var dx = this.Size.X / 2;
        var dy = this.Size.Y / 2;

        this.AbsCenter = gr.Transform.MulPoint(this.Center);
        gr.translate(this.Center.X, this.Center.Y);

        gr.scale(rx, ry);
        gr.rotate(this.Rotation);

        ctx.drawImage(this.ImageIm, -dx / rx, -dy / ry);

        ctx.closePath();
        gr.restore();
    }    
}

class TGraphics {
    Context: CanvasRenderingContext2D;
    Canvas: HTMLCanvasElement;
    Transform: TMat2D = new TMat2D();
    TransformStack: TMat2D[] = [];

    constructor(canvas : HTMLCanvasElement) {
        this.Canvas = canvas;
        this.Context = this.Canvas.getContext("2d");

        var A: TMat2D = this.Transform;
        this.Context.setTransform(A.m11, A.m12, A.m21, A.m22, A.dx, A.dy);
    }

    save() {
        this.TransformStack.push(this.Transform.Copy());
    }

    restore() {
        this.Transform = this.TransformStack.pop();

        var A: TMat2D = this.Transform;
        this.Context.setTransform(A.m11, A.m12, A.m21, A.m22, A.dx, A.dy);
    }

    scale(x: number, y: number) {
        this.Transform.scale(x, y);
        var A: TMat2D = this.Transform;
        this.Context.setTransform(A.m11, A.m12, A.m21, A.m22, A.dx, A.dy);
    }

    translate(x: number, y: number) {
        this.Transform.translate(x, y);
        var A: TMat2D = this.Transform;
        this.Context.setTransform(A.m11, A.m12, A.m21, A.m22, A.dx, A.dy);
    }

    rotate(r: number) {
        this.Transform.rotate(r);
        var A: TMat2D = this.Transform;
        this.Context.setTransform(A.m11, A.m12, A.m21, A.m22, A.dx, A.dy);
    }

    draw() {
        var rct = new TPolygon();
        rct.SetBoundingRectangle(20, 20, 100, 50);
        rct.Draw(this);
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
    MousePosition: TPoint = new TPoint();

    AppInitialize() {
    }

    Rule(self: Object, app: TMyApplication) {
    }
}
