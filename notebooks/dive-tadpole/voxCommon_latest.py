



def initCommonVoxParams(args):

  from socket import gethostname
  import numpy as np
  import colorsys

  nrClust = args.get('nrClust')
  assert nrClust  > 1
  nrRows = int(np.sqrt(nrClust) * 0.95)
  nrCols = int(np.ceil(float(nrClust) / nrRows))
  assert (nrRows * nrCols >= nrClust)

  params = {}
  params['nrOuterIter'] = args.get('nrOuterIt')
  params['nrInnerIter'] = args.get('nrInnerIt')
  params['nrClust'] = nrClust
  params['runIndex'] = args.get('runIndex')
  params['nrProcesses'] = args.get('nrProc')
  params['rangeFactor'] = float(args.get('rangeFactor'))
  params['cluster'] = args.get('cluster')
  params['lambdaMRF'] = 1  # args.lambdaMRF # mrf lambda parameter, only used for MRF model

  # if args.lambdaMRF is not None:
  #   params['lambdaMRF'] = args.lambdaMRF
  #   params['fixMRF'] = True

  params['initClustering'] = args.get('initClustering')

  import aux
  priorNr = aux.setPrior(params, args.get('informPrior'), mean_gamma_alpha=1,
    std_gamma_alpha=args.get('stdGammaAlpha'), mu_beta=0, std_beta=args.get('stdBeta'))

  plotTrajParams = {}
  plotTrajParams['stagingHistNrBins'] = 20
  plotTrajParams['nrRows'] = nrRows
  plotTrajParams['nrCols'] = nrCols
  # plotTrajParams['freesurfPath'] = freesurfPath
  # plotTrajParams['blenderPath'] = blenderPath
  # plotTrajParams['homeDir'] = homeDir
  plotTrajParams['reduceSpace'] = args.get('reduceSpace')
  plotTrajParams['cluster'] = args.get('cluster')
  plotTrajParams['TrajSamplesFontSize'] = 12
  plotTrajParams['TrajSamplesAdjBottomHeight'] = 0.175
  plotTrajParams['trajSamplesPlotLegend'] = True

  if args.get('agg'):
    plotTrajParams['agg'] = True
  else:
    plotTrajParams['agg'] = False

  height = 700

  width = 1300
  if nrClust <= 4:
    heightClust = height / 2
  elif 4 < nrClust <= 6:
    heightClust = int(height * 2/3)
    width = 900
  else:
    heightClust = height

  plotTrajParams['SubfigClustMaxWinSize'] = (width, heightClust)
  plotTrajParams['SubfigVisMaxWinSize'] = (width, height)

  plotTrajParams['clustHuePoints'] = np.linspace(0,1,nrClust,endpoint=False)
  plotTrajParams['clustCols'] = [colorsys.hsv_to_rgb(hue, 1, 1) for hue in plotTrajParams['clustHuePoints']]
  plotTrajParams['legendColsClust'] = min([nrClust, 4])

  return params, plotTrajParams