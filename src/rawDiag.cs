/// adapded from the ThermoFischer `Hello, world!` example provided by Jim Shofstahl 
/// see URL http://planetorbitrap.com/rawfilereader#.WjkqIUtJmL4
/// the ThermoFisher library has to be manual downloaded and installed
/// Please read the License document
/// Witold Wolski <wew@fgcz.ethz.ch> and Christian Panse <cp@fgcz.ethz.ch> and Christian Trachsel
/// 2017-09-25 Zurich, Switzerland
/// 2018-04-24 Zurich, Switzerland
/// 2018-06-04 San Diego, CA, USA added xic option
/// 2018-06-28 added xic and scan option
/// 2018-07-24 bugfix
/// 2018-11-23 added scanFilter option
/// 2019-01-28 extract monoisotopicmZ attribute; include segments in MGF iff no centroid data are availbale
/// 2019-05-28 save info as Yaml
 
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Runtime.ExceptionServices;
using System.Collections;
//using System.Configuration;
//using System.Diagnostics.Eventing;
//using System.Data.Common;
using System.Linq;
//using System.Runtime.DesignerServices;
//using System.Runtime.InteropServices.WindowsRuntime;
//using System.Xml.Schema;
//using System.Runtime.InteropServices;
using ThermoFisher.CommonCore.Data;
using ThermoFisher.CommonCore.Data.Business;
using ThermoFisher.CommonCore.Data.FilterEnums;
using ThermoFisher.CommonCore.Data.Interfaces;
using ThermoFisher.CommonCore.MassPrecisionEstimator;
using ThermoFisher.CommonCore.RawFileReader;



namespace rawDiag
{
    public static class StringExtension
    {
        public static string CleanRawfileTrailerHeader(this string s)
        {
            return(s.Replace(" ", "")
                .Replace("#", "")
                .Replace("m/z", "mZ")
                .Replace("M/Z", "mZ")
                .Replace("(", "")
                .Replace(".", "")
                .Replace(")", "")
                .Replace(":", "")
                .Replace("-", "")
                .Replace("=", ""));
        }
    }
    public class Rawfile {
	    private string _rawfile = "";
	    IRawDataPlus rawFile;

	    public Rawfile(string rawfile) {
		_rawfile = rawfile;
		rawFile = RawFileReaderAdapter.FileFactory(_rawfile);
		rawFile.SelectInstrument(Device.MS, 1);
    	    }

	    public bool check(){
                if (!rawFile.IsOpen || rawFile.IsError)
                {
                    return false;
                }

                if (rawFile.IsError)
                {
                    return false;
                }

                if (rawFile.InAcquisition)
                {
                    return false;
                }
		return true;
	    }

	    public int getFirstScanNumber(){ 
	    	return(rawFile.RunHeaderEx.FirstSpectrum);
	    }

	    public int getLastScanNumber(){ 
	    	return(rawFile.RunHeaderEx.LastSpectrum);
	    }

	    public bool IsCentroidScan(int scanNumber){
            	var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);

            return scanStatistics.IsCentroidScan;
	    }

        public string GetTitle(int scanNumber)
        {
            string s = "File: " + Path.GetFileName(_rawfile) + " SpectrumID: scans: " + scanNumber;
            return s;
        }

        public double GetPepmass(int scanNumber)
        {
            var scanEvent = rawFile.GetScanEventForScanNumber(scanNumber);
            try
            {
                var reaction0 = scanEvent.GetReaction(0);
                return reaction0.PrecursorMass;
            }
            catch
            {
                return -1.0;
            }
        }

        public string GetScanType(int scanNumber)
        {
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
           return scanStatistics.ScanType.ToString();
        }
        public double GetRTinSeconds(int scanNumber)
        {
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            return Math.Round(scanStatistics.StartTime * 60 * 1000) / 1000;
        }

	public double GetBasepeakIntensity(int scanNumber)
	{
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            return  Math.Round(scanStatistics.BasePeakIntensity);
	}



        public string GetMonoisotopicMz(int scanNumber)
        {
            var trailerFields = rawFile.GetTrailerExtraHeaderInformation();
            var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);

            try
            {
                //var reaction0 = scanEvent.GetReaction(0);
                var idx = trailerFields
                    .Select((item, index) => new
                    {
                        name = item.Label.ToString().CleanRawfileTrailerHeader(),
                        Position = index
                    })
                    .First(x => x.name.Contains("MonoisotopicmZ")).Position;


                return scanTrailer.Values.ToArray()[idx];
            }
            catch
            {
                return null;
            }
        }
        public string GetCharge(int scanNumber)
        {
            var trailerFields = rawFile.GetTrailerExtraHeaderInformation();
            //var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            //var scanEvent = rawFile.GetScanEventForScanNumber(scanNumber);
            var scanTrailer = rawFile.GetTrailerExtraInformation(scanNumber);
            
            var idx_CHARGE = trailerFields
                .Select((item, index) => new
                {
                    name = item.Label.ToString(),
                    Position = index
                })
                .First(x => x.name.Contains("Charge State")).Position;
            
            return scanTrailer.Values.ToArray()[idx_CHARGE]; 
        }

        public double[] GetSpectrumIntensities(int scanNumber, string scanFilter)
	{
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            if (scanStatistics.IsCentroidScan)
            {
            	var centroidStream = rawFile.GetCentroidStream(scanNumber, false);
		return centroidStream.Intensities.ToArray();
	    }else{
                var segmentedScan = rawFile.GetSegmentedScanFromScanNumber(scanNumber, scanStatistics);
		return segmentedScan.Intensities.ToArray();
	    }
	    //return null;
	}
        public double[] GetSpectrumMz(int scanNumber, string scanFilter)
        {

            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            if (scanStatistics.IsCentroidScan)
            {
                var centroidStream = rawFile.GetCentroidStream(scanNumber, false);
		return centroidStream.Masses.ToArray();
            }
            else
            {
                var segmentedScan = rawFile.GetSegmentedScanFromScanNumber(scanNumber, scanStatistics);
		return segmentedScan.Positions.ToArray();
               //         Console.WriteLine("  {0} - {1:F4}, {2:F0}", i, segmentedScan.Positions[i], segmentedScan.Intensities[i]);
            }
        }
    }
}
