/// <reference path="../lib/lib.ts"/>
/// <reference path="../lib/runtime.ts"/>

class TMyApplication extends TApplication {
    Razania: TImage = null;

    AppInitialize() {
        var x: number = 100, y: number = 100;

        this.ShapeList = [];

        var circle = new TImage();
        circle.Load("../img/circle.png");
        circle.SetBoundingRectangle(x, y, 80, 80);
        circle.Velocity.X = 2;
        circle.Velocity.Y = 2;
        this.ShapeList.push(circle);
        y += 100;

        // 矩形 枠と塗りつぶし
        var rc1 = new TRectangle();
        rc1.SetBoundingRectangle(x, y, 80, 40);
        rc1.Velocity.X = 1;
        rc1.Velocity.Y = 1;
        rc1.BackgroundColor = "rgb(192, 80, 77)";
        rc1.BorderColor = "#0000FF";
        rc1.BorderWidth = 10;
        this.ShapeList.push(rc1);
        y += 100;

        // 矩形 枠のみ
        var rc2 = new TRectangle();
        rc2.SetBoundingRectangle(x, y, 80, 40);
        rc2.Velocity.X = 2;
        rc2.Velocity.Y = 2;
        rc2.BorderColor = "#0000FF";
        rc2.BorderWidth = 10;
        this.ShapeList.push(rc2);

        // 楕円 枠と塗りつぶし
        var ell1 = new TEllipse();
        ell1.SetBoundingRectangle(x+5, y+5, 70, 30);
        ell1.Velocity.X = 3;
        ell1.Velocity.Y = 3;
        ell1.BackgroundColor = "#FF0000";    // "rgb(255, 255, 0)";
        ell1.BorderColor = "#0000FF";
        ell1.BorderWidth = 10;
        this.ShapeList.push(ell1);
        y += 100;

        // ラベル
        var txt1 = new TLabel();
        txt1.SetBoundingRectangle(x, y, 80, 40);
        txt1.Velocity.X = 4;
        txt1.Velocity.Y = 4;
        txt1.BackgroundColor = "rgb(192, 80, 77)";
        txt1.BorderColor = "#0000FF";
        txt1.BorderWidth = 10;
        txt1.TextColor = "#00FF00";
        txt1.Text = "こんにちは4";
        this.ShapeList.push(txt1);
        y += 100;

        var razania = new TImage();
        razania.Load("../img/food_lasagna_razania.png");
        razania.SetBoundingRectangle(x, y, 100, 741 / 8.0);
        razania.Velocity.X = 2;
        razania.Velocity.Y = 2;
        this.ShapeList.push(razania);
        y += 100;

        var pizza = new TImage();
        pizza.Load("../img/food_pizza_takuhai.png");
        pizza.SetBoundingRectangle(x, y, 100, 712 / 8.0);
        pizza.Velocity.X = 2;
        pizza.Velocity.Y = 2;
        this.ShapeList.push(pizza);

        var grp = new TGroup();
        grp.SetBoundingRectangle(300, 300, 100, 100);
        grp.Velocity.X = 2;
        grp.Velocity.Y = 2;
        this.ShapeList.push(grp);
        y += 100;

        // 矩形 枠と塗りつぶし
        var rc3 = new TRectangle();
        rc3.SetBoundingRectangle(-50, -50, 50, 50);
        rc3.Velocity.X = 1;
        rc3.Velocity.Y = 1;
        rc3.BackgroundColor = "rgb(0, 255, 0)";
        rc3.BorderColor = "#0000FF";
        rc3.BorderWidth = 10;
        rc3.Parent = grp;
        grp.Children.push(rc3);

        // ラベル
        var txt2 = new TLabel();
        txt2.SetBoundingRectangle(0, 0, 50, 50);
        txt2.Velocity.X = 4;
        txt2.Velocity.Y = 4;
        txt2.BackgroundColor = "rgb(192, 80, 77)";
        txt2.BorderColor = "#0000FF";
        txt2.BorderWidth = 10;
        txt2.TextColor = "#FF0000";
        txt2.Text = "今日";
        txt2.Parent = grp;
        grp.Children.push(txt2);
    }

    Rule(self: Object) {
        if (self instanceof TShape) {
            var shape: TShape = <TShape>self;
            if (shape.Parent == null) {
                if (shape.Position.X < 0 || this.Size.X < shape.Position.X + shape.Size.X) {
                    shape.Velocity.X = - shape.Velocity.X;
                }
                if (shape.Position.Y < 0 || this.Size.Y < shape.Position.Y + shape.Size.Y) {
                    shape.Velocity.Y = - shape.Velocity.Y;
                }
                shape.Position.X += shape.Velocity.X;
                shape.Position.Y += shape.Velocity.Y;
            }

            if (shape instanceof TImage) {
                shape.Rotation += 5 * Math.PI / 180;
            }
            else {
                shape.Rotation -= 5 * Math.PI / 180;
            }
        }
    }
}
