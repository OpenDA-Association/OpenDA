using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using System.Linq;
using DHI.Generic.MikeZero;
using DHI.Generic.MikeZero.DFS;

namespace org.openda.dotnet.DHIStochObserver
{

    public enum TimeInterval
    {
        Second = 1400,
        Minute,
        Hour,
        Day,
        Month,
        Year,
    }

    public enum MathType
    {
        Sum,
        Average,
        Min,
        Max
    }

    /// <summary>
    /// Abstract class that handles all direct access to .dfs-files. Uses static methods from DFSWrapper in 
    /// DHI.Generic.MikeZero.DFS.dll as well as direct calls into the ufs.dll
    /// </summary>
    public class DFSBase : IDisposable
    {
        #region Calls directly into ufs.dll because the wrapped call does not work on vista due to something with strings.
        const string UFSDll = "ufs.dll";  // Name of dll. Should be in path


        /// <summary>
        /// Call directly into ufs.dll because the wrapped call does not work on vista due to something with strings.
        /// </summary>
        /// <param name="HeaderPointer"></param>
        /// <param name="Projection"></param>
        /// <param name="longitude"></param>
        /// <param name="Latitude"></param>
        /// <param name="Orientation"></param>
        /// <returns></returns>
        [DllImport(UFSDll, CharSet = CharSet.None, CallingConvention = CallingConvention.StdCall)]
        internal extern static int dfsGetGeoInfoUTMProj(IntPtr HeaderPointer, ref IntPtr Projection, ref double longitude, ref double Latitude, ref double Orientation);

        #endregion

        //Keeps track of the data in the buffer
        private int _currentTimeStep = 0;
        private int _currentItem = 1;
        protected float[] dfsdata; //Buffer used to fill data into
        private double[] _times; //this array contains the timesteps from non equidistant calendar axis in file units. Used only for writing
        private double? _deleteValue;

        protected IntPtr _filePointer = IntPtr.Zero;
        protected IntPtr _headerPointer = IntPtr.Zero;
        protected bool _initializedForWriting = false;
        private TimeInterval timeStepUnit = TimeInterval.Second;

        protected TimeSpan _timeStep = TimeSpan.Zero;
        public TimeAxisType _timeAxis { get; set; }
        protected SpaceAxisType _spaceAxis;

        protected string AbsoluteFileName;
        private string _filename;

        protected int _numberOfLayers = 1;
        protected int _numberOfColumns = 1;
        protected int _numberOfRows = 1;

        protected double _xOrigin = 0;
        protected double _yOrigin = 0;
        protected double _orientation = 0;
        protected double _gridSize = 1;
        protected List<DateTime> _timesteps = new List<DateTime>();

        private int _status;

        private int NumberOfTimeStepsWritten = 0;


        #region Constructors


        public DFSBase()
        {
        }

        /// <summary>
        /// Creates a new .dfs file
        /// </summary>
        /// <param name="FileName"></param>
        /// <param name="Title"></param>
        /// <param name="NumberOfItems"></param>
        public DFSBase(string DFSFileName, int NumberOfItems)
            : this()
        {
            _filename = DFSFileName;
            AbsoluteFileName = Path.GetFullPath(DFSFileName);

            this.NumberOfItems = NumberOfItems;
            _initializedForWriting = true;

        }

        protected void CopyItemInfo(DFSBase TemplateDFS)
        {
            for (int i = 0; i < TemplateDFS.Items.Count(); i++)
            {
                Items[i].Name = TemplateDFS.Items[i].Name;
                Items[i].EumItem = TemplateDFS.Items[i].EumItem;
                Items[i].EumUnit = TemplateDFS.Items[i].EumUnit;
                Items[i].ValueType = TemplateDFS.Items[i].ValueType;
            }
        }


        /// <summary>
        /// Opens an existing dfs-file
        /// </summary>
        /// <param name="DFSFileName"></param>
        public DFSBase(string DFSFileName)
            : this()
        {
            _filename = DFSFileName;
            AbsoluteFileName = Path.GetFullPath(DFSFileName);

            try
            {
                DfsDLLWrapper.dfsFileRead(AbsoluteFileName, out _headerPointer, out _filePointer);
            }
            catch (Exception e)
            {
                return; //Not a valid file. 
            }
            NumberOfItems = DfsDLLWrapper.dfsGetNoOfItems(_headerPointer);


            string eum_unit = "";
            int unit = 0;
            int data_type = 0;
            int item_type = 0;

            float x = 0;
            float y = 0;
            float z = 0;

            float dx = 0;
            float dy = 0;
            float dz = 0;

            IntPtr name = new IntPtr();


            //Reads the projection
            LastStatus = dfsGetGeoInfoUTMProj(_headerPointer, ref name, ref _xOrigin, ref _yOrigin, ref _orientation);

            //Reads the space axis
            _spaceAxis = (SpaceAxisType)DfsDLLWrapper.dfsGetItemAxisType(FirstItem.ItemPointer);

            //Now read axes info dependent on the type of axis
            switch (_spaceAxis)
            {
                case SpaceAxisType.CurveLinearD2:
                    break;
                case SpaceAxisType.CurveLinearD3:
                    break;
                case SpaceAxisType.EqD0:
                    break;
                case SpaceAxisType.EqD1:
                    break;
                case SpaceAxisType.EqD2:       //DFS2 from MikeShe
                    DfsDLLWrapper.dfsGetItemAxisEqD2(FirstItem.ItemPointer, out item_type, out eum_unit, out _numberOfColumns, out _numberOfRows, out x, out y, out dx, out dy);
                    break;
                case SpaceAxisType.EqD3: //DFS3 from MikeShe
                    DfsDLLWrapper.dfsGetItemAxisEqD3(FirstItem.ItemPointer, out item_type, out eum_unit, out _numberOfColumns, out _numberOfRows, out _numberOfLayers, out x, out y, out z, out dx, out dy, out dz);
                    break;
                case SpaceAxisType.NeqD1:
                    var coords = new Coords[1];
                    DfsDLLWrapper.dfsGetItemAxisNeqD1(FirstItem.ItemPointer, out unit, out eum_unit, out data_type, out coords);
                    break;
                case SpaceAxisType.NeqD2:
                    break;
                case SpaceAxisType.NeqD3:
                    break;
                case SpaceAxisType.Undefined:
                    break;
                default:
                    break;
            }

            _gridSize = dx;

            //Prepares an array of floats to recieve the data
            dfsdata = new float[_numberOfColumns * _numberOfRows * _numberOfLayers];

            //Now look at time axis
            _timeAxis = (TimeAxisType)DfsDLLWrapper.dfsGetTimeAxisType(_headerPointer);
            string startdate = "";
            string starttime = "";
            double tstart = 0;
            double tstep = 0;
            int nt = 0;
            int tindex = 0;

            switch (_timeAxis)
            {
                case TimeAxisType.TimeEquidistant: //Some DFS2 here
                    DfsDLLWrapper.dfsGetEqTimeAxis(_headerPointer, out unit, out eum_unit, out tstart, out tstep, out nt, out tindex);
                    break;
                case TimeAxisType.CalendarEquidistant: //Dfs2 and dfs3 here
                    DfsDLLWrapper.dfsGetEqCalendarAxis(_headerPointer, out startdate, out starttime, out unit, out eum_unit, out tstart, out tstep, out nt, out tindex);
                    if (unit == 1400)
                        _timeStep = TimeSpan.FromSeconds(tstep);
                    else if (unit == 1401) //This is a guess
                        _timeStep = TimeSpan.FromMinutes(tstep);
                    else if (unit == 1402)
                        _timeStep = TimeSpan.FromHours(tstep);
                    break;
                case TimeAxisType.TimeNonEquidistant: //This has not been tested
                    DfsDLLWrapper.dfsGetNeqTimeAxis(_headerPointer, out unit, out eum_unit, out tstart, out tstep, out nt, out tindex);
                    break;
                case TimeAxisType.CalendarNonEquidistant://Only dfs0 can have varying time steps
                    DfsDLLWrapper.dfsGetNeqCalendarAxis(_headerPointer, out startdate, out starttime, out unit, out eum_unit, out tstart, out tstep, out nt, out tindex);
                    break;
                case TimeAxisType.Undefined:
                    break;
                default:
                    break;
            }

            NumberOfTimeStepsWritten = nt;
            timeStepUnit = (TimeInterval)unit;

            if (_timeAxis == TimeAxisType.CalendarNonEquidistant | _timeAxis == TimeAxisType.TimeEquidistant)
                _times = new double[nt];

            if (startdate != "" & starttime != "")
            {
                _timesteps.Add(DateTime.Parse(startdate).Add(TimeSpan.Parse(starttime)));
            }
            else //Time equidistant files enter here. 
                _timesteps.Add(new DateTime(2002, 1, 1));

            //Now build the list of timesteps
            for (int i = 1; i < nt; i++)
            {
                if (_timeAxis == TimeAxisType.CalendarNonEquidistant) //dfs0 with time varying.
                    _timesteps.Add(_timesteps[0].Add(GetTimeSpan(i)));
                else
                    _timesteps.Add(_timesteps[i - 1].Add(_timeStep));
            }


        }

        #endregion


        public virtual void CopyFromTemplate(DFSBase dfs)
        {
            _timeAxis = dfs._timeAxis;
            if (dfs._timeAxis == TimeAxisType.CalendarEquidistant || dfs._timeAxis == TimeAxisType.TimeEquidistant)
            {
                this.TimeOfFirstTimestep = dfs.TimeOfFirstTimestep;
                this.TimeStep = dfs.TimeStep;
            }
            this.DeleteValue = dfs.DeleteValue;

            if (DfsDLLWrapper.dfsIsFileCompressed(dfs._headerPointer))
            {
                var en = DfsDLLWrapper.dfsGetEncodeKeySize(dfs._headerPointer);

                int[] xkey = new int[en];
                int[] ykey = new int[en];
                int[] zkey = new int[en];

                DfsDLLWrapper.dfsGetEncodeKey(dfs._headerPointer, xkey, ykey, zkey);
                DfsDLLWrapper.dfsSetEncodeKey(_headerPointer, xkey, ykey, zkey, en);
            }
        }

        #region Read methods

        //Gets the timespan for a time step using readitemtimestep. Should only be used with CalendarNonEquidistant
        protected TimeSpan GetTimeSpan(int TimeStep)
        {
            TimeSpan ts = TimeSpan.Zero;
            double time = 0;

            while (_currentTimeStep < TimeStep)
            {
                IncrementItemTimeStep();
                DfsDLLWrapper.dfsSkipItem(_headerPointer, _filePointer);
            }

            DfsDLLWrapper.dfsReadItemTimeStep(_headerPointer, _filePointer, out time, dfsdata);
            IncrementItemTimeStep();

            switch (this.timeStepUnit)
            {
                case TimeInterval.Second:
                    ts = TimeSpan.FromSeconds(time);
                    break;
                case TimeInterval.Minute:
                    ts = TimeSpan.FromMinutes(time);
                    break;
                case TimeInterval.Hour:
                    ts = TimeSpan.FromHours(time);
                    break;
                case TimeInterval.Day:
                    break;
                case TimeInterval.Month:
                    break;
                case TimeInterval.Year:
                    break;
                default:
                    break;
            }
            return ts;
        }

        /// <summary>
        /// Returns the zero-based index of the TimeStep closest to the TimeStamp. If the timestamp falls exactly between two timestep the smallest is returned.
        /// If the TimeStamp is before the first timestep 0 is returned.
        /// If the TimeStamp is after the last timestep the index of the last timestep is returned
        /// </summary>
        /// <param name="TimeStamp"></param>
        /// <returns></returns>
        public int GetTimeStep(DateTime TimeStamp)
        {
            if (TimeStamp < TimeSteps.First() || NumberOfTimeSteps == 1)
                return 0;
            int TimeStep;
            //fixed timestep
            if (_timeAxis == TimeAxisType.CalendarEquidistant)
                TimeStep = (int)Math.Round(TimeStamp.Subtract(TimeSteps.First()).TotalSeconds / _timeStep.TotalSeconds, 0);
            //Variabale timestep
            else
            {
                //Last timestep is known
                if (TimeStamp >= TimeSteps[NumberOfTimeSteps - 1])
                    return NumberOfTimeSteps - 1;

                int i = 1;
                //Loop the timesteps
                while (TimeStamp > TimeSteps[i])
                {
                    i++;
                }
                //Check if last one was actually closer
                if (TimeSteps[i].Subtract(TimeStamp) < TimeStamp.Subtract(TimeSteps[i - 1]))
                    return i;
                else
                    return i - 1;
            }
            return Math.Min(NumberOfTimeSteps, TimeStep);
        }

        /// <summary>
        /// Moves to the timestep and item
        /// Returns true if it was actually necessary to move
        /// Note that it is not possible to move backwards into something that has been written without 
        /// </summary>
        /// <param name="TimeStep"></param>
        /// <param name="Item"></param>
        /// <returns></returns>
        private bool MoveToItemTimeStep(int TimeStep, int Item)
        {
            TimeStep = Math.Min(TimeStep, NumberOfTimeStepsWritten);
            Item = Math.Min(Item, NumberOfItems);
            if (TimeStep != _currentTimeStep || Item != _currentItem)
            {
                _currentTimeStep = TimeStep;
                _currentItem = Item;

                if (TimeStep == NumberOfTimeStepsWritten)
                {
                    DfsDLLWrapper.dfsFindItemDynamic(_headerPointer, _filePointer, TimeStep - 1, NumberOfItems); //Spools to last item
                    DfsDLLWrapper.dfsSkipItem(_headerPointer, _filePointer); // now at end
                    _currentItem = 1;
                    return true;
                }
                else
                {
                    //Spools to the correct Item and TimeStep
                    DfsDLLWrapper.dfsFindItemDynamic(_headerPointer, _filePointer, TimeStep, Item);
                    return true;
                }
            }
            return false;
        }


        private bool EndOfFile
        {
            get
            {
                return _currentTimeStep == NumberOfTimeStepsWritten;
            }
        }

        private void IncrementItemTimeStep()
        {

            _currentItem++;
            if (_currentItem > NumberOfItems)
            {
                _currentItem = 1;
                _currentTimeStep++;
            }

        }

        /// <summary>
        /// Reads data for the TimeStep and Item and fills them into the buffer.
        /// Time steps counts from 0 and Item from 1.
        /// </summary>
        /// <param name="TimeStep"></param>
        /// <param name="Item"></param>
        public float[] ReadItemTimeStep(int TimeStep, int Item)
        {
            double time = 0;
            MoveToItemTimeStep(TimeStep, Item);
            DfsDLLWrapper.dfsReadItemTimeStep(_headerPointer, _filePointer, out time, dfsdata);
            IncrementItemTimeStep();
            return dfsdata;
        }

        #endregion

        #region Math methods
        /// <summary>
        /// Multiplies a factor to the values in the item and timestep
        /// </summary>
        /// <param name="TimeStep"></param>
        /// <param name="Item"></param>
        /// <param name="factor"></param>
        public void MultiplyItemTimeStep(int TimeStep, int Item, double factor)
        {
            ReadItemTimeStep(TimeStep, Item);
            for (int i = 0; i < dfsdata.Count(); i++)
                if (dfsdata[i] != (float)DeleteValue)
                    dfsdata[i] *= (float)factor;
            WriteItemTimeStep(TimeStep, Item, dfsdata);
        }

        /// <summary>
        /// Adds a factor to the values in the item and timestep
        /// </summary>
        /// <param name="TimeStep"></param>
        /// <param name="Item"></param>
        /// <param name="factor"></param>
        public void AddToItemTimeStep(int TimeStep, int Item, double factor)
        {
            ReadItemTimeStep(TimeStep, Item);
            for (int i = 0; i < dfsdata.Count(); i++)
                if (dfsdata[i] != (float)DeleteValue)
                    dfsdata[i] += (float)factor;
            WriteItemTimeStep(TimeStep, Item, dfsdata);
        }

        /// <summary>
        /// Sums the values of the items to the selected time interval and puts them in the new dfs file
        /// Assumes that the there are delete values at the same places in all items and timesteps!
        /// </summary>
        /// <param name="Items"></param>
        /// <param name="df"></param>
        /// <param name="SumTim"></param>
        public void TimeAggregation(int[] Items, DFSBase df, TimeInterval SumTim, int Tsteps, MathType mathtype)
        {
            Dictionary<int, float[]> BufferData = new Dictionary<int, float[]>();

            List<int> NonDeleteIndex = null;
            List<int> DeleteIndex = null;

            //Initialize all items
            foreach (var j in Items)
                BufferData[j] = new float[dfsdata.Count()];


            DateTime LastPrint = TimeSteps[0];

            bool PrintNow = false;

            int tstepCounter = 0;

            //Loop the time steps
            for (int i = 0; i < NumberOfTimeSteps; i++)
            {
                tstepCounter++;
                switch (SumTim)
                {
                    case TimeInterval.Year:
                        PrintNow = (LastPrint.Year + Tsteps == TimeSteps[i].Year);
                        break;
                    case TimeInterval.Month:
                        int nextmonth = LastPrint.Month + Tsteps;
                        if (nextmonth > 12)
                            nextmonth -= 12;
                        PrintNow = (nextmonth == TimeSteps[i].Month);
                        break;
                    case TimeInterval.Day:
                        PrintNow = ((TimeSteps[i].Subtract(LastPrint) >= TimeSpan.FromDays(Tsteps)));
                        break;
                    default:
                        break;
                }

                //Now print summed values and empty buffer
                if (PrintNow)
                {
                    foreach (var j in Items)
                    {
                        if (mathtype == MathType.Average) //Average, and a division with the number of time steps is required
                            foreach (var n in NonDeleteIndex)
                                BufferData[j][n] = BufferData[j][n] / ((float)tstepCounter);

                        df.WriteItemTimeStep(df.NumberOfTimeSteps, j, BufferData[j]);

                        foreach (var n in NonDeleteIndex)
                            if (mathtype == MathType.Min)
                                BufferData[j][n] = float.MaxValue;
                            else if (mathtype == MathType.Max)
                                BufferData[j][n] = float.MinValue;
                            else
                                BufferData[j][n] = 0;

                    }
                    tstepCounter = 0;
                    LastPrint = TimeSteps[i];
                    PrintNow = false;
                }
                //Sum all items
                foreach (var j in Items)
                {
                    ReadItemTimeStep(i, j);
                    if (DeleteIndex == null) //For the first time step build a list of the non-delete values
                    {
                        DeleteIndex = new List<int>();
                        NonDeleteIndex = new List<int>();
                        for (int k = 0; k < dfsdata.Count(); k++)
                        {
                            if (dfsdata[k] == DeleteValue)
                            {
                                DeleteIndex.Add(k);
                                foreach (var n in Items)
                                    BufferData[n][k] = dfsdata[k]; //Set the delete values for all items
                            }
                            else
                                NonDeleteIndex.Add(k);
                            foreach (var n in Items)
                            {
                                if (mathtype == MathType.Min)
                                    BufferData[n][k] = float.MaxValue;
                                else if (mathtype == MathType.Max)
                                    BufferData[n][k] = float.MinValue;
                                else
                                    BufferData[n][k] = 0;
                            }
                        }
                    }
                    var arr = BufferData[j];
                    foreach (int k in NonDeleteIndex)
                        if (mathtype == MathType.Min)
                            arr[k] = Math.Min(arr[k], dfsdata[k]);
                        else if (mathtype == MathType.Max)
                            arr[k] = Math.Max(arr[k], dfsdata[k]);
                        else
                            arr[k] += dfsdata[k];
                }
            }
            //print the last summed values
            foreach (var j in Items)
            {
                if (mathtype == MathType.Average) //If not sum it is average and a division with the number of time steps is required
                    foreach (var n in NonDeleteIndex)
                        BufferData[j][n] = BufferData[j][n] / ((float)tstepCounter);
                df.WriteItemTimeStep(df.NumberOfTimeSteps, j, BufferData[j]);
            }
        }


    

        #endregion

        #region Write methods

        /// <summary>
        /// Writes data for the TimeStep and Item
        /// </summary>
        /// <param name="TimeStep"></param>
        /// <param name="Item"></param>
        public void WriteItemTimeStep(int TimeStep, int Item, float[] data)
        {
            if (!_initializedForWriting)
                InitializeForWriting();

            if (_filePointer == IntPtr.Zero)
                CreateFile();

            MoveToItemTimeStep(TimeStep, Item);

            double time = 0;
            if (_timeAxis == TimeAxisType.CalendarNonEquidistant & _currentTimeStep > 0)
            {
                TimeSpan ts = TimeSteps[_currentTimeStep].Subtract(TimeSteps[0]);
                switch (timeStepUnit)
                {
                    case TimeInterval.Second:
                        time = ts.TotalSeconds;
                        break;
                    case TimeInterval.Minute:
                        time = ts.TotalMinutes;
                        break;
                    case TimeInterval.Hour:
                        time = ts.TotalHours;
                        break;
                    default:
                        break;
                }
            }

            if (EndOfFile)
            {
                if (_currentItem == NumberOfItems)
                    NumberOfTimeStepsWritten++;
                AppendTimeStep(TimeSteps.Last().Add(_timeStep));
            }

            //Writes the data
            DfsDLLWrapper.dfsWriteItemTimeStep(_headerPointer, _filePointer, time, data);
            IncrementItemTimeStep();
        }

        protected virtual void AppendTimeStep(DateTime Time)
        {
            if (_timeAxis != TimeAxisType.CalendarNonEquidistant & _currentTimeStep > 0)
            {
                TimeSteps.Add(Time);
            }
        }

        /// <summary>
        /// Opens the file for writing. First closes the file since it has already been opened for reading
        /// </summary>
        protected void InitializeForWriting()
        {
            Dispose(false);
            DfsDLLWrapper.dfsFileEdit(_filename, out _headerPointer, out _filePointer);
            _initializedForWriting = true;

            for (int i = 0; i < NumberOfItems; i++)
            {
                var ip = DfsDLLWrapper.dfsItemD(_headerPointer, i + 1);
                Items[i].ItemPointer = ip;
            }

        }

        private void CreateFile()
        {
            WriteGeoInfo();
            WriteTime();
            foreach (Item I in Items)
            {
                WriteItemInfo(I);
                if (_spaceAxis == SpaceAxisType.EqD2)
                    DfsDLLWrapper.dfsSetItemAxisEqD2(I.ItemPointer, 1000, _numberOfColumns, _numberOfRows, 0, 0, (float)_gridSize, (float)_gridSize);
                else if (_spaceAxis == SpaceAxisType.EqD3)
                    DfsDLLWrapper.dfsSetItemAxisEqD3(I.ItemPointer, 1000, _numberOfColumns, _numberOfRows, _numberOfLayers, 0, 0, 0, (float)_gridSize, (float)_gridSize, (float)_gridSize);
                else if (_spaceAxis == SpaceAxisType.EqD0)
                    DfsDLLWrapper.dfsSetItemAxisEqD0(I.ItemPointer, 1000);
            }
            DfsDLLWrapper.dfsFileCreate(FileName, _headerPointer, out _filePointer);
        }

        protected void WriteGeoInfo()
        {
            if (!_initializedForWriting)
                InitializeForWriting();
            DfsDLLWrapper.dfsSetGeoInfoUTMProj(_headerPointer, "NON-UTM", _xOrigin, _yOrigin, _orientation);
            foreach (Item I in Items)
            {
                WriteItemInfo(I);
                if (_spaceAxis == SpaceAxisType.EqD2)
                    DfsDLLWrapper.dfsSetItemAxisEqD2(I.ItemPointer, 1000, _numberOfColumns, _numberOfRows, 0, 0, (float)_gridSize, (float)_gridSize);
            }
        }

        internal void WriteItemInfo(Item I)
        {
            if (!_initializedForWriting)
                InitializeForWriting();

            DfsDLLWrapper.dfsSetItemInfo(_headerPointer, I.ItemPointer, (int)I.EumItem, I.Name, (int)I.EumUnit, DfsSimpleType.Float);
            DfsDLLWrapper.dfsSetItemValueType(I.ItemPointer, I.ValueType);
        }

        /// <summary>
        /// Writes timestep and starttime
        /// </summary>
        protected void WriteTime()
        {
            if (!_initializedForWriting)
                InitializeForWriting();
            switch (_timeAxis)
            {
                case TimeAxisType.CalendarEquidistant:
                    DfsDLLWrapper.dfsSetEqCalendarAxis(_headerPointer, TimeSteps.First().ToString("yyyy-MM-dd"), TimeSteps.First().ToString("HH:mm:ss"), (int)timeStepUnit, 0, _timeStep.TotalSeconds, 0);
                    break;
                case TimeAxisType.CalendarNonEquidistant:
                    DfsDLLWrapper.dfsSetNeqCalendarAxis(_headerPointer, TimeSteps.First().ToString("yyyy-MM-dd"), TimeSteps.First().ToString("HH:mm:ss"), (int)timeStepUnit, 0, 0);

                    break;
                case TimeAxisType.TimeEquidistant:
                    break;
                case TimeAxisType.TimeNonEquidistant:
                    break;
                case TimeAxisType.Undefined:
                    break;
                default:
                    break;
            }
        }

        #endregion

        #region Properties


        private Item[] items;

        /// <summary>
        /// Gets the items
        /// </summary>
        public Item[] Items
        {
            get
            {
                if (items == null)
                {
                    items = new Item[NumberOfItems];

                    //Gets the pointers to the items
                    for (int i = 0; i < NumberOfItems; i++)
                    {
                        items[i] = new Item(DfsDLLWrapper.dfsItemD(_headerPointer, i + 1), this, i + 1);
                    }
                }
                return items;
            }
        }


        /// <summary>
        /// Gets the status code from the last call by DFSWrapper
        /// </summary>
        protected int LastStatus
        {
            get { return _status; }
            set
            {
                _status = value;
                if (_status != 0)
                {
                    string error = "fejl";
                }
            }
        }

        /// <summary>
        /// Gets the first item. There should always be at least one item
        /// </summary>
        public Item FirstItem
        {
            get
            {
                return Items[0];
            }
        }



        /// <summary>
        /// Gets an array with the timesteps.
        /// </summary>
        public virtual IList<DateTime> TimeSteps
        {
            get
            {
                return _timesteps;
            }
        }

        /// <summary>
        /// Gets and sets the date and time of the first time step.
        /// </summary>
        public DateTime TimeOfFirstTimestep
        {
            get
            {
                return TimeSteps.First();
            }
            set
            {
                if (TimeSteps.Count == 0)
                    TimeSteps.Add(value);
                else
                    TimeSteps[0] = value;
                WriteTime();
            }
        }

        /// <summary>
        /// Gets and sets the size of a time step
        /// </summary>
        public TimeSpan TimeStep
        {
            get
            {
                return _timeStep;
            }
            set
            {
                if (_timeStep != value)
                {
                    _timeStep = value;
                    WriteTime();
                }
            }
        }

        /// <summary>
        /// Gets the DeleteValue from the DFS-file
        /// </summary>
        public double DeleteValue
        {
            get
            {
                if (!_deleteValue.HasValue)
                {
                    _deleteValue = DfsDLLWrapper.dfsGetDeleteValFloat(_headerPointer);
                }
                return _deleteValue.Value;
            }
            set
            {

                if (_deleteValue.HasValue)
                    if (_deleteValue.Value == value)
                        return;

                _deleteValue = value;
                DfsDLLWrapper.dfsSetDeleteValFloat(_headerPointer, (float)value);
            }
        }


        /// <summary>
        /// Gets the FileName
        /// </summary>
        public string FileName
        {
            get
            {
                return _filename;
            }
        }


        /// <summary>
        /// Gets the number of timesteps
        /// </summary>
        public int NumberOfTimeSteps
        {
            get
            {
                return TimeSteps.Count;
            }
        }

        /// <summary>
        /// Gets the number of items
        /// </summary>
        public int NumberOfItems { get; private set; }

        #endregion

        #region Dispose methods

        /// <summary>
        /// Override of the Dispose method in DFSFileInfo which probably does not account for finalization
        /// </summary>
        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        protected virtual void Dispose(bool disposing)
        {
            if (disposing)
            {
                dfsdata = null;
            }
            if (_headerPointer != IntPtr.Zero)
                DfsDLLWrapper.dfsFileClose(_headerPointer, ref _filePointer);
        }

        /// <summary>
        /// Destructor called when the object is garbage collected.
        /// </summary>
        ~DFSBase()
        {
            // Simply call Dispose(false).
            Dispose(false);
        }

        #endregion

    }

    public class Item
    {
        internal IntPtr ItemPointer { get; set; }
        private eumUnit _eumUnit = eumUnit.eumUUnitUndefined;
        private string eumUnitString;
        private string _name;
        private eumItem _eumitem = eumItem.eumIItemUndefined;
        private DFSBase _dfs;
        private DataValueType valueType;

        public int ItemNumber { get; private set; }
        public int NumberOfElements { get; private set; }

        internal Item(IntPtr ItemPointer, DFSBase DFS, int Number)
        {
            ItemNumber = Number;
            _dfs = DFS;
            int item_type = 0;
            int data_type = 0;
            int value_type = 0;

            IntPtr name = new IntPtr();
            IntPtr Eum = new IntPtr();

            this.ItemPointer = ItemPointer;
            DfsDLLAccess.dfsGetItemInfo_(ItemPointer, out item_type, ref name, ref Eum, out data_type);
            DfsDLLAccess.dfsGetItemValueType(ItemPointer, out value_type);
            NumberOfElements = (int)DfsDLLAccess.dfsGetItemElements(ItemPointer);


            valueType = (DataValueType)value_type;
            _name = (Marshal.PtrToStringAnsi(name));
            eumUnitString = Marshal.PtrToStringAnsi(Eum);
            if (item_type != 0)
                _eumitem = (eumItem)item_type;


        }

        /// <summary>
        /// Gets and sets the name
        /// </summary>
        public string Name
        {
            get
            {
                return _name;
            }
            set
            {
                if (value != _name)
                {
                    _name = value;
                    _dfs.WriteItemInfo(this);
                }
            }
        }

        /// <summary>
        /// Gets and sets the Eum Item
        /// </summary>
        public eumItem EumItem
        {
            get
            {
                return _eumitem;
            }
            set
            {
                if (_eumitem != value)
                {
                    _eumitem = value;
                    _eumUnit = PossibleUnits.First();
                    _dfs.WriteItemInfo(this);
                }
            }
        }

        /// <summary>
        /// Gets a list of units possible for the selected EUM Item
        /// </summary>
        public eumUnit[] PossibleUnits
        {
            get
            {
                return EUMWrapper.GetItemAllowedUnits(EumItem);
            }
        }

        /// <summary>
        /// Gets the eum quantity
        /// </summary>
        public eumQuantity EumQuantity
        {
            get
            {
                return new eumQuantity(EumItem, EumUnit);
            }
        }

        /// <summary>
        /// Gets and sets the eum unit.
        /// Note that possible units depend on the EUMItem. Setting to an impossible unit will set the unit to first possible type
        /// </summary>
        public eumUnit EumUnit
        {
            get
            {
                if (_eumUnit == eumUnit.eumUUnitUndefined)
                {
                    int u = 0;
                    bool w = EUMWrapper.GetUnitTag(this.eumUnitString, out u); //This call is very expensive.
                    _eumUnit = (eumUnit)u;
                }
                return _eumUnit;
            }
            set
            {
                if (!PossibleUnits.Contains(value))
                    _eumUnit = PossibleUnits.First();
                _eumUnit = value;
            }
        }

        /// <summary>
        /// Gets and sets the data value type
        /// </summary>
        public DataValueType ValueType
        {
            get
            {
                return valueType;
            }
            set
            {
                if (valueType != value)
                {
                    valueType = value;
                    _dfs.WriteItemInfo(this);
                }
            }
        }


        public override string ToString()
        {
            return Name;
        }

    }
}
