using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace org.openda.dotnet.DHIStochObserver
{
    public interface IXYLayerPoint
    {
        double X { get; }

        double Y { get; }

        int Layer { get; }
    }
}
