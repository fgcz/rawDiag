/// adapded from the ThermoFischer `Hello, world!` example provided by Jim Shofstahl 
/// see URL http://planetorbitrap.com/rawfilereader#.WjkqIUtJmL4
/// the ThermoFisher library has to be manual downloaded and installed
/// Please read the License document
/// Christian Panse <cp@fgcz.ethz.ch> and Christian Trachsel
/// 2019-05-29 initial using rDotNet; added class rawDiag 
 
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Runtime.ExceptionServices;
using System.Collections;
using System.Linq;
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
        public double GetRTinSeconds(int scanNumber) {
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

        public int GetPolarity(int scanNumber)
        {
            
            var scanFilter = rawFile.GetFilterForScanNumber(scanNumber);

            if (scanFilter.Polarity.ToString() == "Positive") return 1;
            
            return -1;
        }

        
        public int GetMsLevel(int scanNumber)
        {
            
            var scanFilter = rawFile.GetFilterForScanNumber(scanNumber);
            
            if (scanFilter.MSOrder.ToString() == "Ms") return 1;
            else if (scanFilter.MSOrder.ToString() == "Ms2") return 2; 
            else if (scanFilter.MSOrder.ToString() == "Ms3") return 3; 
            else return -1;
        }

        // TODO(cp): implement it 
        public int[] GetMsLevel()
        {
            int[] rv = {1,2,2,3,4};
            return rv;

        }
        
        public string GetCharge(int scanNumber)
        {
            var trailerFields = rawFile.GetTrailerExtraHeaderInformation();
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
	}
        public double[] GetSpectrumNoises(int scanNumber, string scanFilter)
	{
            var scanStatistics = rawFile.GetScanStatsForScanNumber(scanNumber);
            if (scanStatistics.IsCentroidScan)
            {
                var centroidStream = rawFile.GetCentroidStream(scanNumber, false);
		return centroidStream.Noises.ToArray();
	    }else{
		return null;
	    }
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
            }
        }
    }
}
