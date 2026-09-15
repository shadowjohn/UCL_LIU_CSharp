using System;
using System.Drawing;

namespace uclliu
{
    public sealed class WindowDragGesture
    {
        private readonly int minimumDragPixels;
        private Point pointerDownLocation;
        private Point windowDownLocation;
        private bool hasStarted;

        public WindowDragGesture(int minimumDragPixels)
        {
            this.minimumDragPixels = Math.Max(1, minimumDragPixels);
        }

        public bool IsDragging { get; private set; }

        public void Begin(Point pointerScreenLocation, Point windowLocation)
        {
            pointerDownLocation = pointerScreenLocation;
            windowDownLocation = windowLocation;
            hasStarted = true;
            IsDragging = false;
        }

        public Point Move(Point pointerScreenLocation)
        {
            if (!hasStarted)
            {
                return windowDownLocation;
            }

            int offsetX = pointerScreenLocation.X - pointerDownLocation.X;
            int offsetY = pointerScreenLocation.Y - pointerDownLocation.Y;
            if (!IsDragging && Math.Abs(offsetX) < minimumDragPixels && Math.Abs(offsetY) < minimumDragPixels)
            {
                return windowDownLocation;
            }

            IsDragging = true;
            return new Point(windowDownLocation.X + offsetX, windowDownLocation.Y + offsetY);
        }

        public bool Complete()
        {
            bool shouldSuppressClick = IsDragging;
            hasStarted = false;
            IsDragging = false;
            return shouldSuppressClick;
        }
    }

}
