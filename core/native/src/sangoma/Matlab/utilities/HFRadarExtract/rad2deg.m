%% Copyright (C) 2004 Andrew Collier <abcollier@users.sourceforge.net>
%%
%% This program is free software; it is distributed in the hope that it
%% will be useful, but WITHOUT ANY WARRANTY; without even the implied
%% warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
%% the GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this file; see the file COPYING.  If not, see
%% <http://www.gnu.org/licenses/>.

%% -*- texinfo -*-
%% @deftypefn {Function File} {} @var{anglout} = rad2deg(@var{anglin})
%%
%% Converts angles input in radians to the equivalent in degrees.
%% @end deftypefn

%% Author: Andrew Collier <abcollier@users.sourceforge.net>

function anglout = rad2deg(anglin)
  anglout = anglin * 180 / pi;
end

%% http://www.mathworks.com/access/helpdesk/help/toolbox/map/functionlist.shtml
