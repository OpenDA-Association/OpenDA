using OpenDA.DotNet.Interfaces;

namespace OpenDA.DotNet.SDK
{
	public class ResultsJ2N
	{
		public static void PutMessage(string message)
		{
			org.openda.utils.Results.putMessage(message);
		}

		public static void PutValue(string message, double value, MessageType messageType)
		{
			// TODO: handle message type
			org.openda.utils.Results.putValue(message, value, 1, null, org.openda.interfaces.IResultWriter.OutputLevel.Normal,org.openda.interfaces.IResultWriter.MessageType.Step);
		}

		public static void PutValue(string message, IVector vector, MessageType messageType)
		{
			// TODO: handle message type
            org.openda.utils.Results.putValue(message, vector, vector.Size, null, org.openda.interfaces.IResultWriter.OutputLevel.Normal, org.openda.interfaces.IResultWriter.MessageType.Step);
		}
	}
}
