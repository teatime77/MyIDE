Public Class TMyApplication
    Inherits TApplication
    Public CircleR As Double = 80
    Public cnt As Double = 0

    Public Overrides Sub AppInitialize()
        Dim x As Double = 100, y As Double = 100

        Dim circle = New TImage()
        circle.Load("../img/circle.png")
        circle.SetBoundingRectangle(x, y, 80, 80)
        circle.Velocity.X = 2
        circle.Velocity.Y = 2
        ShapeList.push(circle)
        y += 100
        Dim rc1 = New TRectangle()
        rc1.SetBoundingRectangle(x, y, 80, 40)
        rc1.Velocity.X = 1
        rc1.Velocity.Y = 1
        rc1.BackgroundColor = "rgb(192, 80, 77)"
        rc1.BorderColor = "#0000FF"
        rc1.BorderWidth = 10
        ShapeList.push(rc1)
        y += 100
        Dim rc2 = New TRectangle()
        rc2.SetBoundingRectangle(x, y, 80, 40)
        rc2.Velocity.X = 2
        rc2.Velocity.Y = 2
        rc2.BorderColor = "#0000FF"
        rc2.BorderWidth = 10
        ShapeList.push(rc2)
        Dim ell1 = New TEllipse()
        ell1.SetBoundingRectangle(x + 5, y + 5, 70, 30)
        ell1.Velocity.X = 3
        ell1.Velocity.Y = 3
        ell1.BackgroundColor = "#FF0000"
        ell1.BorderColor = "#0000FF"
        ell1.BorderWidth = 10
        ShapeList.push(ell1)
        y += 100
        Dim txt1 = New TLabel()
        txt1.SetBoundingRectangle(x, y, 80, 40)
        txt1.Velocity.X = 4
        txt1.Velocity.Y = 4
        txt1.BackgroundColor = "rgb(192, 80, 77)"
        txt1.BorderColor = "#0000FF"
        txt1.BorderWidth = 10
        txt1.TextColor = "#00FF00"
        txt1.Text = "こんにちは4"
        ShapeList.push(txt1)
        y += 100
        Dim razania = New TImage()
        razania.Load("../img/food_lasagna_razania.png")
        razania.SetBoundingRectangle(x, y, 100, 741 / 8.0)
        razania.Velocity.X = 2
        razania.Velocity.Y = 2
        ShapeList.push(razania)
        y += 100
        Dim pizza = New TImage()
        pizza.Load("../img/food_pizza_takuhai.png")
        pizza.SetBoundingRectangle(x, y, 100, 712 / 8.0)
        pizza.Velocity.X = 2
        pizza.Velocity.Y = 2
        ShapeList.push(pizza)
        Dim grp = New TGroup()
        grp.SetBoundingRectangle(300, 300, 100, 100)
        grp.Velocity.X = 2
        grp.Velocity.Y = 2
        ShapeList.push(grp)
        y += 100
        Dim rc3 = New TRectangle()
        rc3.SetBoundingRectangle(-25, -25, 50, 50)
        rc3.Velocity.X = 1
        rc3.Velocity.Y = 1
        rc3.BackgroundColor = "rgb(0, 255, 0)"
        rc3.BorderColor = "#0000FF"
        rc3.BorderWidth = 10
        rc3.Parent = grp
        grp.Children.push(rc3)
        Dim txt2 = New TLabel()
        txt2.SetBoundingRectangle(25, 25, 50, 50)
        txt2.Velocity.X = 4
        txt2.Velocity.Y = 4
        txt2.BackgroundColor = "rgb(192, 80, 77)"
        txt2.BorderColor = "#0000FF"
        txt2.BorderWidth = 10
        txt2.TextColor = "#FF0000"
        txt2.Text = "今日"
        txt2.Parent = grp
        grp.Children.push(txt2)
    End Sub

    <_Invariant()> Public Overrides Sub Rule(self As Object, app As TMyApplication)
        If TypeOf self Is TShape Then
            With CType(self, TShape)
                If .Parent Is Nothing Then
                    If .Center.X - .Radius < 0 OrElse app.Size.X < .Center.X + .Radius Then
                        .Velocity.X = - .Velocity.X
                    End If
                    If .Center.Y - .Radius < 0 OrElse app.Size.Y < .Center.Y + .Radius Then
                        .Velocity.Y = - .Velocity.Y
                    End If
                    .Center.X += .Velocity.X
                    .Center.Y += .Velocity.Y

                    .Test.Position = New TPoint()
                Else

                    Dim p As Double = .Parent.Test.Position.X
                End If

                If .AbsCenter.Distance(app.MousePosition) <= .Radius Then
                    If TypeOf self Is TImage Then
                        .Rotation += 19 * Math.PI / 180
                    Else
                        .Rotation -= 19 * Math.PI / 180
                    End If
                Else
                    .Rotation += 5 * Math.PI / 180
                End If

                If TypeOf self Is TImage Then
                    With CType(self, TImage)
                        app.cnt += 1
                        console.log("pos:" + .AbsCenter.X + " " + .AbsCenter.Y + " " + .ImageIm.src)
                        If .ImageIm.src = "http://localhost:17623/img/circle.png" Then
                            .Size.X = app.CircleR + 20 * Math.Sin(Math.PI * app.cnt / 180)
                            .Size.Y = app.CircleR + 20 * Math.Cos(Math.PI * app.cnt / 180)
                        End If

                    End With

                ElseIf TypeOf self Is TGroup Then
                    With CType(self, TGroup)
                        Dim v1 = (From x In .Children Select x.Center.X).ToArray()
                        'console.log("Center X:" + v1)

                        Dim vshape = From x In .Children Where TypeOf x Is TImage
                        .BoundingRectangle.Position.X = Aggregate img In vshape Into Min(img.BoundingRectangle.Position.X)
                        Dim vx = From x In .Children Where TypeOf x Is TImage Select x.BoundingRectangle.Position.X
                        .BoundingRectangle.Position.X = Aggregate x In vx Into Min(x)
                        .BoundingRectangle.Position.X = Aggregate img In (From x In .Children Where TypeOf x Is TImage) Into Min(img.BoundingRectangle.Position.X)
                        .BoundingRectangle.Position.X = Aggregate img In .Children Where TypeOf img Is TImage Into Min(img.BoundingRectangle.Position.X)

                    End With
                End If

            End With
        End If
    End Sub
End Class
