
%station 1
d3d_qp('openfile','notFiltered/trih-Lake2D.dat')
d3d_qp('colour',[ 0 0 1 ])
d3d_qp('station','S1')
d3d_qp('quickview')
d3d_qp('openfile','stochModel/model/trih-Lake2D.def')
d3d_qp('colour',[ 0 1 0 ])
d3d_qp('addtoplot')
d3d_qp('openfile','stochObserver/trih-Lake2D.dat')
d3d_qp('colour',[ 0 0 0 ])
d3d_qp('addtoplot')

%station 4
d3d_qp('selectfile','notFiltered/trih-Lake2D.dat')
d3d_qp('colour',[ 0 0 1 ])
d3d_qp('station','S4')
d3d_qp('quickview')
d3d_qp('selectfile','stochModel/model/trih-Lake2D.def')
d3d_qp('colour',[ 0 1 0 ])
d3d_qp('addtoplot')
d3d_qp('selectfile','stochObserver/trih-Lake2D.dat')
d3d_qp('colour',[ 0 0 0 ])
d3d_qp('addtoplot')

