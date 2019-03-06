# wget https://www.gsc-europa.eu/sites/default/files/2017-06-22.xml
$LOAD_PATH.unshift File.dirname(__FILE__)
require 'nokogiri'
require 'Ephemeris'


def radians(degrees)
    return degrees * Math::PI / 180.0
end

def get_field(sv_xml, name)
    sv_xml.xpath(name).first.content.to_f
end

#     <almanac>
#         <aSqRoot>0.0195312</aSqRoot>
#         <ecc>0.000305176</ecc>
#         <deltai>0.00561523</deltai>
#         <omega0>0.253387</omega0>
#         <omegaDot>-1.86265e-09</omegaDot>
#         <w>0.517151</w>
#         <m0>0.526733</m0>
#         <af0>0.00671387</af0>
#         <af1>-3.27418e-11</af1>
#         <iod>2</iod>
#         <t0a>366000</t0a>
#         <wna>2</wna>
#     </almanac>
class GalileoAlmanac < Ephemeris
    # http://www.navipedia.net/index.php/GPS_and_Galileo_Satellite_Coordinates_Computation
    
    def initialize(svid, sv_xml)
        @svprn = svid
        # https://www.gsc-europa.eu/almanac-information
        @roota = Math.sqrt(29600.0e3)*(1.0 - get_field(sv_xml, "aSqRoot"))
        @ecc = get_field(sv_xml, "ecc")
        @i0 = radians(54.0) + Math::PI*get_field(sv_xml, "deltai")
        
        @omega_c = get_field(sv_xml, "omega0")*Math::PI
        @omegadot = get_field(sv_xml, "omegaDot")*Math::PI
        @omega = get_field(sv_xml, "w")*Math::PI
        @m0 = get_field(sv_xml, "m0")*Math::PI
        @af0 = get_field(sv_xml, "af0")
        @af1 = get_field(sv_xml, "af1")
        @af2 = 0.0
        @iod = get_field(sv_xml, "iod")
        @toe = get_field(sv_xml, "t0a")
        @wna = get_field(sv_xml, "wna")
        
        @cis = 0.0
        @cic = 0.0
        @cus = 0.0
        @cuc = 0.0
        @crs = 0.0
        @crc = 0.0
        @deltan = 0.0
        @idot = 0.0
    end
    
    def to_s
        "Galileo Ephemeris SV=#{@svprn}, toc=#{@toc}, a0=#{@a0}, ecc=#{@ecc}, m0=#{@m0}, roota=#{@roota}"
    end

end


class GalileoEphemerides
    
    def initialize(fname)
        @svs = []
        doc = Nokogiri::XML(File.open(fname))
        svs = doc.xpath("//svAlmanac")
        svs.each do |sv_xml|
            svid = sv_xml.xpath("SVID").first.content
            almanac = sv_xml.xpath("almanac")
            @svs << GalileoAlmanac.new(svid, almanac)
        end
    end

    def get_prn_list()
        ret = []
        @svs.each do |eph| 
            ret << eph.svprn
        end
        return ret.uniq.sort
    end

    #!\brief Get a valid ephemeris for the prn, and GpsTime t
    def get_ephemeris(prn, t)
        fail = true;
        best_e = nil

        @svs.each do |eph| 
            if (eph.svprn == prn)
                best_e = eph;
                fail = false;
            end
        end
        throw Exception.new("No suitable ephemeris found SV=#{prn}, t=#{t}") if (fail)
        return best_e
    end
end

