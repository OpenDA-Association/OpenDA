

using System;
using System.Reflection;
using OpenDA.DotNet.Interfaces;

namespace OpenDA.DotNet.Bridge
{
    public class StochObserverFactory
    {
        private String _workingDirPath;
        private String _libraryName;
        private String _className;
        private String _configFile;
        Assembly _dllAssembly;
        IStochObserver _myLoadClass;
    

        public void Initialize(String workingDirPath, string[] arguments){
            if (arguments.Length != 3)
			{
				throw new Exception(GetType() + " expects three arguments: name dll, name class and configuration file");
			}
            _workingDirPath = workingDirPath;
            _libraryName    = arguments[0];
            _className      = arguments[1];
            _configFile     = arguments[2];
            /* Load dll */
            _dllAssembly = Assembly.LoadFile(_libraryName);

        }
        public IStochObserver GetInstance(int outputLevel)
        {
           // IStochObserver newObserver = (IStochObserver)Activator.CreateInstance(_myLoadClass);
            object instance = _dllAssembly.CreateInstance(_className, true);
            if (instance == null)
            {
                throw new Exception("Cannot create instance of class " + _className);
            }


            IStochObserver newObserver = (IStochObserver)instance;


            String[] arguments = new String[1];
            arguments[0] = _configFile;
            newObserver.Initialize(_workingDirPath, arguments);
            return newObserver;
        }
    }
}
