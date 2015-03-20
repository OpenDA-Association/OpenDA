
using System;

namespace OpenDA.DotNet.SDK
{
	public class ConfigTreeJ2N
	{
		private readonly org.openda.utils.ConfigTree _javaConfigTree;

		public ConfigTreeJ2N(org.openda.utils.ConfigTree javaConfigTree)
		{
			_javaConfigTree = javaConfigTree;
		}

		public ConfigTreeJ2N(string workingDirPath, string configstring)
		{
			_javaConfigTree = new org.openda.utils.ConfigTree(new java.io.File(workingDirPath), configstring);
		}

		public string GetContentstring(string path)
		{
			return _javaConfigTree.getContentString(path);
		}

		public string GetAsstring(string path, string defaultValue)
		{
			return _javaConfigTree.getAsString(path, defaultValue);
		}
	}
}
