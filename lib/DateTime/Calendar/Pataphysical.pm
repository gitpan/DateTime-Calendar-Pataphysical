package DateTime::Calendar::Pataphysical;

use strict;

use vars qw($VERSION);

$VERSION = '0.04';

use DateTime::Duration;
use DateTime::Locale;
use Params::Validate qw/validate SCALAR OBJECT/;

sub _floor {
    my $x  = shift;
    my $ix = int $x;
    if ($ix <= $x) {
        return $ix;
    } else {
        return $ix - 1;
    }
}

use overload ( 'fallback' => 1,
               '<=>' => '_compare_overload',
               'cmp' => '_compare_overload',
               '-' => '_subtract_overload',
               '+' => '_add_overload',
             );

{
    my $DefaultLocale;
    sub DefaultLocale {
        my $class = shift;

        if (@_) {
            my $lang = shift;

            DateTime::Locale->load($lang);

            $DefaultLocale = $lang;
        }

        return $DefaultLocale;
    }
}
__PACKAGE__->DefaultLocale('French');

sub new {
    my $class = shift;
    my %p = validate( @_,
                      { year  => {type => SCALAR},
                        month => {type => SCALAR, default => 1},
                        day   => {type => SCALAR, default => 1},
                        rd_secs   => { type => SCALAR, default => 0},
                        rd_nano   => { type => SCALAR, default => 0},
                        locale  => { type => SCALAR | OBJECT,
                                       default => $class->DefaultLocale },
                      } );

    my $self = bless \%p, $class;
    $self->{locale} = DateTime::Locale->load($p{locale})
        unless (ref $self->{locale});

    return $self;
}

sub clone {
    my $self = shift;

    return bless {%$self}, ref $self;
}

sub set
{
    my $self = shift;
    my %p = validate( @_,
                      { year     => { type => SCALAR, optional => 1 },
                        month    => { type => SCALAR, optional => 1 },
                        day      => { type => SCALAR, optional => 1 },
                        locale => { type => SCALAR | OBJECT, optional => 1 },
                      } );

    if (exists $p{locale} && ! ref $p{locale}) {
        $p{locale} = DateTime::Locale->load($p{locale})
    }

    $self->{$_} = $p{$_} for keys %p;
    return $self;
}

sub truncate {
    my $self = shift;
    my %p = validate( @_,
                      { to =>
                        { regex => qr/^(?:year|month|day)$/ },
                      },
                    );
    foreach my $f ( qw( day month year ) ) {
        last if $p{to} eq $f;
        $self->{$f} = 1;
    }
    return $self;
}

sub locale { $_[0]->{locale} }

sub is_leap_year {
    my $self = shift;
    my $year = $self->{year};
    $year++ if $year < 0;
    if ($year % 4 != 3 or ($year % 100 == 27 and $year % 400 != 127)) {
        return 0;
    } else {
        return 1;
    }
}

sub year    { $_[0]->{year} }

sub month   { $_[0]->{month} }
*mon = \&month;          

sub month_0 { $_[0]->{month}-1 }
*mon_0 = \&month_0;

sub day_of_month { $_[0]->{day} }
*day  = \&day_of_month;
*mday = \&day_of_month;

sub day_of_month_0 { $_[0]->{day} - 1 }
*day_0  = \&day_of_month_0;
*mday_0 = \&day_of_month_0;

sub month_name {
    return (qw/Absolu Haha As Sable D�cervelage Gueules P�dale Clinamen
               Palotin Merdre Gidouille Tatane Phalle/)[$_[0]->{month}-1];
}

sub day_of_week {
    my $self = shift;

    if ($self->{day} == 29) {
        return undef;
    } else {
        return 1 + ($self->{day}-1) % 7;
    }
}

sub day_of_week_0 {
    my $self = shift;

    if ($self->{day} == 29) {
        return undef;
    } else {
        return +($self->{day}-1) % 7;
    }
}

sub day_name {
    my $self = shift;

    if ($self->{day} == 29) {
        my $name = 'hunyadi';
        my $n = $self->{locale}->day_names->[0];
        $name = ucfirst $name if $n eq ucfirst $n;
        return $name;
    } else {
        return $self->{locale}->day_names->[($self->day_of_week_0 || 7)-1];
    }
}

sub week_number {
    my $self = shift;

    if ($self->{day} == 29) {
        return undef;
    } else {
        return 4*($self->{month} - 1) + int(($self->{day} - 1)/7) + 1;
    }
}

sub week_year { $_[0]->year }
    
sub week { $_[0]->week_year, $_[0]->week_number }

sub day_of_year {
    my $self = shift;

    return $self->{day} + ($self->{month}-1) * 29;
}

sub day_of_year_0 {
    my $self = shift;

    return $self->{day} + ($self->{month}-1) * 29 - 1;
}

sub ymd {
    my ($self, $sep) = @_;
    $sep = '-' unless defined $sep;

    return sprintf( "%0.3d%s%0.2d%s%0.2d",
                    $self->{year}, $sep,
                    $self->{month}, $sep,
                    $self->{day} );
}
*date = \&ymd;

sub mdy {
    my ($self, $sep) = @_;
    $sep = '-' unless defined $sep;

    return sprintf( "%0.2d%s%0.2d%s%0.3d",
                    $self->{month}, $sep,
                    $self->{day}, $sep,
                    $self->{year} );
}
sub dmy {
    my ($self, $sep) = @_;
    $sep = '-' unless defined $sep;

    return sprintf( "%0.2d%s%0.2d%s%0.3d",
                    $self->{day}, $sep,
                    $self->{month}, $sep,
                    $self->{year} );
}

sub datetime {
    my $self = shift;

    # EP = Ere Pataphysique
    return $self->ymd() . 'EP';
}

my %formats =
    ( 'A' => sub { $_[0]->day_name },
      'B' => sub { $_[0]->month_name },
      'C' => sub { int( $_[0]->year / 100 ) },
      'd' => sub { sprintf( '%02d', $_[0]->day_of_month ) },
      'D' => sub { $_[0]->strftime( '%m/%d/%y' ) },
      'e' => sub { sprintf( '%2d', $_[0]->day_of_month ) },
      'F' => sub { $_[0]->ymd('-') },
      'j' => sub { $_[0]->day_of_year },
      'm' => sub { sprintf( '%02d', $_[0]->month ) },
      'n' => sub { "\n" },
      't' => sub { "\t" },
      'u' => sub { $_[0]->day_of_week || 'H' },
      'U' => sub { my $w = $_[0]->week_number;
                   defined $w ? sprintf('%02d', $w) : '  ' },
      'w' => sub { my $dow = $_[0]->day_of_week;
                   defined $dow ? $dow-1 : 'H' },
      'y' => sub { sprintf( '%02d', substr( $_[0]->year, -2 ) ) },
      'Y' => sub { return $_[0]->year },
      '%' => sub { '%' },
      '*' => sub { $_[0]->feast },
    );
$formats{W} = $formats{V} = $formats{U};

sub strftime {
    my ($self, @r) = @_;

    foreach (@r) {
        s/%([%*A-Za-z])/ $formats{$1} ? $formats{$1}->($self) : $1 /ge;
        return $_ unless wantarray;
    }
    return @r;
}

sub last_day_of_month {
    my $class = shift;
    my %p = validate( @_,
                      { year   => { type => SCALAR },
                        month  => { type => SCALAR },
                        locale  => { type => SCALAR | OBJECT, optional => 1 },
                      }
                    );
    $p{day} = 29;
    return $class->new(%p);
}

sub is_imaginary {
    my $self = shift;

    return $self->{day} == 29 && $self->{month} != 11 &&
           ($self->{month} != 6 or !$self->is_leap_year);
}

sub utc_rd_values {
    my $self = shift;

    return if $self->is_imaginary;

    my ($year, $month, $day) = @{$self}{qw/year month day/};
    $year++ if $year < 0;

    my $cyear = $year;
    $cyear++ if $month > 6;
    $day++ if $month > 11;

    my $rd = 683984 +                   # 7 September 1873 = 28 Phalle '0'
             ($year-1) * 365 +          # normal years: 365 real days
             _floor( .25 * $cyear) -    # leap years
             _floor(($cyear+72)/100) +  # century years
             _floor(($cyear+272)/400 ) +
             + ($month - 1) * 28 + $day;
    return ($rd, $self->{rd_secs}, $self->{rd_nano});
}

sub utc_rd_as_seconds {
    my $self = shift;
    my ($rd_days, $rd_secs, $rd_nano) = $self->utc_rd_values;

    if (defined $rd_days) {
        return $rd_days*24*60*60 + $rd_secs + $rd_nano * 1e-9;
    } else {
        return undef;
    }
}

sub from_object {
    my $class = shift;
    my %p = validate( @_,
                      { object => { type => OBJECT,
                                    can => 'utc_rd_values',
                                  },
                        locale => { type => SCALAR | OBJECT,
                                      default => $class->DefaultLocale },
                      },
                       );

    $p{object} = $p{object}->clone->set_time_zone( 'floating' )
                                if $p{object}->can( 'set_time_zone' );

    my ( $rd_days, $rd_secs, $rd_nano ) = $p{object}->utc_rd_values;

    my ($y, $m, $d) = $class->_rd2ymd( $rd_days );

    return $class->new( year => $y, month => $m, day => $d,
                        rd_secs => $rd_secs||0, rd_nano => $rd_nano||0,
                        locale => $p{locale} );
}

sub _rd2ymd {
    my ($class, $rd) = @_;

    # Algorithm similar to the one on
    # http://home.capecod.net/~pbaum/date/injdalg2.htm
    # for the gregorian calendar

    # Number of days since 1 Pedale 127 (day after first extra leap day) =
    # 24-02-2000
    $rd -= 730173;

    my $a = _floor(($rd-0.25)/(100*365.2425));
    my $b = $rd - 0.25 + $a - _floor($a/4);
    my $y = _floor($b/365.25);
    my $d = $rd + $a - _floor($a/4) - _floor(365.25*$y);

    my $m;
    if ($d < 5*28 + 1) {        # Before 29 Gidouille
        $m = _floor(($d-1)/28);
        $d -= $m * 28;
    } elsif ($d == 5*28 + 1) {  # 29 Gidouille
        $m = 4;
        $d = 29;
    } elsif ($d < 366) {        # Before 29 Gueules
        $m = _floor(($d-2)/28);
        $d -= $m*28 + 1;
    } else {                    # 29 Gueules (leap day)
        $m = 12;
        $d = 29;
    }

    $y += 127;
    $m += 7;
    if ($m > 13) {
        $m -= 13;
        $y ++;
    }

    # There is no year 0
    if ($y <= 0) {
        $y--;
    }

    return $y, $m, $d;
}

sub from_epoch {
    my $class = shift;
    my %p = validate( @_,
                      { epoch => { type => SCALAR },
                        locale => { type => SCALAR | OBJECT,
                                      default => $class->DefaultLocale },
                      }
                    );

    my $rd = int($p{epoch}/(24*60*60) + 719163);
    
    my ($y, $m, $d) = $class->_rd2ymd( $rd );

    return $class->new( year => $y, month => $m, day => $d,
                        locale => $p{locale} );
}

sub now { shift->from_epoch( epoch => (scalar time), @_ ) }

sub _add_overload {
    my ($dt, $dur, $reversed) = @_;
    ($dur, $dt) = ($dt, $dur) if $reversed;

    my $new = $dt->clone;
    $new->add_duration($dur);
    return $new;
}

sub _subtract_overload
{
    my ( $date1, $date2, $reversed ) = @_;
    ($date1, $date2) = ($date2, $date1) if $reversed;

    if ( UNIVERSAL::isa($date2, 'DateTime::Duration') ) {
        my $new = $date1->clone;
        $new->add_duration( $date2->inverse );
        return $new;
    } else {
        return $date1->subtract_datetime($date2);
    }
}

sub add {return shift->add_duration(DateTime::Duration->new(@_)) }

sub subtract { return shift->subtract_duration(DateTime::Duration->new(@_)) }

sub subtract_duration { return $_[0]->add_duration( $_[1]->inverse ) }

sub add_duration {
    my ($self, $dur) = @_;

    my %deltas = $dur->deltas;

    $self->{year}++ if $self->{year} < 0;

    $self->{day} += $deltas{days} if $deltas{days};
    $self->{month} += $deltas{months} if $deltas{months};

    if ($self->{day} < 1 or $self->{day} > 29) {
        $self->{month} += _floor(($self->{day}-1)/29);
        $self->{day} %= 29;
    }
    if ($self->{month} < 1 or $self->{month} > 13) {
        $self->{year} += _floor(($self->{month}-1)/13);
        $self->{month} %= 13;
    }

    $self->{year}-- if $self->{year} <= 0;

    return $self;
}

sub subtract_datetime {
    my ($self, $dt) = @_;

    my ($syear, $dyear) = ($self->year, $dt->year);
    $_ < 0 and $_++ for $syear, $dyear;

    my $days_diff = ($syear       - $dyear    ) * 377 +
                    ($self->month - $dt->month) * 29 +
                    ($self->day   - $dt->day  );
    return DateTime::Duration->new( days => $days_diff );
}

use constant INFINITY     =>       100 ** 100 ** 100 ;
use constant NEG_INFINITY => -1 * (100 ** 100 ** 100);
            
sub _compare_overload
{       
    # note: $_[1]->compare( $_[0] ) is an error when $_[1] is not a
    # DateTime (such as the INFINITY value)
    return $_[2] ? - $_[0]->compare( $_[1] ) : $_[0]->compare( $_[1] );
}

sub compare
{
    my ($class, $dt1, $dt2) = ref $_[0] ? (undef, @_) : @_;

    return undef unless defined $dt2;

    return -1 if ! ref $dt2 && $dt2 == INFINITY;
    return  1 if ! ref $dt2 && $dt2 == NEG_INFINITY;

    $dt2 = $class->from_object( object => $dt2 )
        unless $dt2->isa('DateTime::Calendar::Pataphysical');

    return $dt1->year <=> $dt2->year || $dt1->month <=> $dt2->month ||
           $dt1->day  <=> $dt2->day;
}


my @feasts;

sub feast {
    return $feasts[ $_[0]->day_of_year_0 ][1];
}

sub type_of_feast {
    return $feasts[ $_[0]->day_of_year_0 ][0];
}

# Feasts from
# http://perso.wanadoo.fr/mexiqueculture/nouvelles6-latumba.htm
@feasts = map [/(.) (.+)/], split /\n+/, <<EOF;
1 Nativit� d'Alfred Jarry
4 St Ptyx, silentiaire (Abolition de)
4 St Ph�nix, solipsiste et St Hyx, factotum
4 St Lucien de Samosate, voyageur
4 St Bardamu, voyageur
4 Ste V�rola, assistante sociale
4 St Alambic, abstracteur
3 Absinthe, ci-devant St Alfred
4 Descente du St Esprit (de Vin)
v Dilution
4 Ste Pur�e, sportswoman
v Vide
4 St Canterel, l'illuminateur
4 St Sophrotatos l'Arm�nien, pataphysicien
3 �thernit�
4 St Ibicrate le G�om�tre, pataphysicien
v C�phalorgie
v Fl�tes de Pan
4 Stes Grues, ophiophiles
4 Ste M�lusine, souillarde de cuisine
4 St Venceslas, duc
2 Emmanuel Dieu
4 Ste Varia-Miriam, amphibie
4 Sts Rakirs et Rastrons, porte-c�telettes
4 Nativit� de Sa Magnificence Opach
4 St Joseb, notaire � la mode de Bretagne
4 Stes Gigolette et Gaufrette, dogaresses
v Xylostomie
v Le Jet Musical

2 L'�ge du Dr Faustroll
4 Dissolution d'E. Poe, dinomythurge
4 St Gibus, franc-ma�on
4 Ste Berthe de Courri�re, �g�rie
4 Ste Belgique, nourrice
4 Ste Tourte, lyrique et Ste B�vue, sociologue
4 St Prout, abb�
2 F�te du Haha
v Tautologie
4 St Panmuphle, huissier
4 Sortie de St L. Cranach, apocalypticien
4 St Cosinus, savant
4 Bse Fenouillard, sainte famille
4 Exhibition de la Daromphe
3 Nativit� de l'OEstre, artificier
4 Ste Vadrouille, embl�me
4 St Homais d'Aquin, prudhomme
4 Nativit� de Sa Magnificence le baron Mollet (St Pipe)
4 St Rapha�l, ap�ritif et philistin
3 Strangulation de Bosse-de-Nage
3 Zimzoum de Bosse-de-Nage
2 R�surrection de Bosse-de-Nage
3 Chapeau de Bosse-de-Nage
4 St Cl. Terrasse, musicien des Phynances
4 St J.-P. Brisset, philologue, prince des penseurs
4 Comm�moration du Cure-dent
1 Occultation d'Alfred Jarry
4 Fuite d'Ablou
v Mar�e Terrestre

3 Nativit� de Pantagruel
4 Ste Rrose S�lavy, h�ro�ne
4 Couronnement de Lord Patchogue, miroitier
4 St Cravan, boxeur
4 St Van Meegeren, faussaire
4 St Omnibus, satyre
4 St Cyrano de Bergerac, explorateur
3 St Rimbe, oisif
v �quarrissage pour tous
4 St Abstrait, bourreau
4 St Ossian, barde postiche
3 Dispute du Signe + et du Signe -
3 Moustaches du Dr Faustroll
4 St P. Bonnard, peintre des Phynances
1 Navigation du Dr Faustroll
4 St Cap, captain
4 St Pangloss, humoriste passif
4 St Chambernac, pauvriseur
4 St Courtial des P�reires, a�rostier et inventeur
4 St Olibrius, augure
4 St Possible, schizophr�ne
2 St Lautr�amont
4 St Quincey, critique d'art
4 St Berbiguier, martyr
4 St Lewis Carroll, professeur
4 St Mensonger, �v�que
4 Ste Visit�, fille du pr�c�dent
4 Nativit� de St Swift, chanoine
v Travers�e du Miroir

3 Noces de Balkis et de Salomon
4 St Doublemain, id�ologue
4 St Phlegmon, doctrinaire
4 Ste Barbe (femme �), femme-canon
4 Ste Savate, avocate
4 St Navet et Ste Perruque, humanistes
4 St Birbe, juge
2 Conception du P. Ubu (A. J.)
4 St Sagouin, homme d'�tat
1 Exaltation d'Ubu Roi (Ubu d'hiver)
4 Nativit� de St Grabbe, scherziste
4 Ste Choupe, m�re de famille
4 St Flaive, concierge
4 Don Quichotte, champion du monde
2 Khurmookum du Dr Faustroll
4 St Nul, exempt
4 St Moyen, fran�ais
4 Ste Lurette, joconde
3 Gravidit� de M�re Ubu
4 St Sabre, allopathe
4 Ste Tape, pompette
1 C�sar - Antechrist
4 Ste Viole, vierge et martyre
4 Ste Pochet�e, gouvernante
3 Nativit� de l'Arch�opt�ryx
4 Monsieur Sisyphe
4 St Tic, conjoint
4 St Cervelas, penseur
v Aleph

3 St Alaodine, virtuose
4 Sts Hassassins, praticiens
4 Astu
1 D�cervelage
4 Sts Giron, Pile et Cotice, palotins
4 Sts Polonais, prol�taires
4 Sts For�ats, poliorc�tes
3 St Bordure, capitaine
4 Dormition de Jacques Vach�, interpr�te
v Drapaud (�rection du)
4 St Eustache, lib�rateur
4 St Landru, gyn�cologue
4 St Guillotin, m�decin
4 Sts 4 Sans-Cou, enchanteurs
3 Conscience d'Ubu
4 St Mauvais, sujet
4 St Mandrin, po�te et philosophe
4 Sts Pirates et Flibustiers, thaumaturges
4 St et Ste Cartouche, v�t�rinaires
4 St Outlaw, aristocrate
1 Chaire du Dr FaustrolL
2 Ostention du B�ton � Physique
4 St Tank, animal
4 St Weidman, patriarche
4 St Petiot, expert
v Escrime
4 Sts Chemins de fer, assassins
v Repopulation
v Lit de Procruste

3 D�pucelage de M�re Ubu
4 St Sigisb�e, eunuque
4 St Anthropo�de, policier
4 Ste Goule ou Gudule, institutrice
4 Ste Gale, abbesse
4 Ste Touche, postulante
4 St Gueule, abb�
3 F�te de la Chandelle Verte
4 Ste Cr�pe, la�que
4 St Pr�servatif, bedeau
4 St Baobab, c�libataire
4 St Membre, compilateur
v Copulation
4 Nativit� de St J. Verne, globe-trotter en chambre
v Alice au Pays des Merveilles
4 St M�nchhausen, baron
4 Le B�trou, th�urge
4 Nativit� de St Deibler, prestidigitateur
4 St Sade �s liens
4 St Lafleur, valet
v Lavement
2 St Sexe, stylite
4 Occultation de St J. Torma, euphoriste
4 Conversion de St Matorel, bateleur
4 Ste Marmelade, inspir�e
3 L'Amour Absolu, deliquium
4 Ste Tabagie, cosmog�ne
4 Sts Hylactor et Pamphagus
v Mouvement Perp�tuel

3 �rection du Surm�le
4 St Andr� Marcueil, asc�te cycliste
4 St Ellen, hile
4 St Michet, id�aliste
4 St Ouducul, trouv�re
4 Vers Belges
4 St Gavroche, forain
3 La Machine � Inspirer l'Amour
4 St Remezy, �v�que in partibus
4 Nativit� de St Tancr�de, jeune homme
4 Testament de P. Uccello, le mal illumin�
4 St Hari Seldon, psychohistorien galactique
4 Ste Valburge, succube
v Sabbat
3 Sts Adelphes, �sot�ristes
4 Sts Templiers, adeptes
4 St Dricarpe, pros�lyte
4 St Nosocome, carabin
4 Ste Goutte, f�te militaire
4 Ste Cuisse, dame patronnesse
4 St Inscrit, Converti
2 St Sengle, d�serteur
4 St Masquarade, uniforme
4 Nativit� de St St�phane, faune
4 St Poligraf Poligrafovitch, chien
4 St P�le, mineur
3 St Valens, fr�re onirique
v D�dicace du Tripode
4 Bse Escampette, dynamiteuse

3 St Ablou, page et St Haldern, duc
4 Sts Hiboux, ma�tres-chanteurs
4 La Mandragore, solan�e andro�de
4 St Pagne, confident
4 Sts Aster et Vulpian, violateurs du N�ant
4 St Ganym�de, professionnel
v La Main de Gloire
3 La Machine � Peindre
4 Ste Trique, lunatique
4 R�mission des Poissons
4 St Maquereau, intercesseur
4 St Georges Dazet, poulpe au regard de soie
4 Nativit� de Maldoror, corsaire aux cheveux d'or
4 Sortie d'A. D�rer, herm�tiste
* Invention de la 'Pataphysique
4 Exit St Domenico Theotocopouli, el Greco
4 St Hi�ronymus Bosch, d�monarque
v Les 27 �tres Issus des Livres Pairs
4 St Barbeau, procureur et Ste Morue, juste
v Capture du Fourneau
4 St Docteur Moreau, insulaire
2 F�te des Poly�dres
v Locus Solus
4 St Tupetu de Tupetu, organisateur de loteries
4 Exit St Goya, alchimiste
4 St Escargot, sybarite
4 Ste Hure de Chastet�, p�nitente
4 St Turgescent, iconoclaste
v Cymbalum Mundi

3 Sts Crocodiles, crocodiles
4 F�te des �cluses
4 Sts Trolls, pantins
4 Ste Susan Calvin, docteur
4 Ste Poign�e, veuve et Ste Jutte, recluse
4 Ste Oneille, gourgandine
4 St F�n�on �s Liens
3 St Bougrelas, prince
4 Sts Boleslas et Ladislas, polonais
4 St Forficule, Barnabite
v Explosion du Palotin
v R�probation du Travail
4 Esquive de St L�onard (de Vinci), illusionniste
4 St �quivoque, sans-culotte
3 Adoration du Pal
4 D�ploration de St Achras, �leveur de Poly�dres
4 St Macrotatoure, caudataire
v Canotage
4 Occultation de St Gauguin, oc�anide
4 St Ti Belot, s�ide
4 Occultation de Sa Magnificence le Dr Sandomir
2 Sts Palotins des Phynances
4 Sts Quatrezoneilles, Herdanpo, Mousched-Gogh, palotins
4 Ste Lumelle, �cuy�re
4 Sts Potassons, acolythes
4 Ste Pr�tentaine, rosi�re
4 St Foin, coryph�e
4 Nativit� de St Satie, Grand Parcier de l'�glise d'Art
v Erratum

3 Accouchement de Ste Jeanne, papesse
v Le Moutardier du Pape
4 St Si�ge, sous-pape
4 Nativit� de St H. Rousseau, douanier
4 St Crouducul, troupier
4 St Cucufat, m�c�ne
4 Nativit� de M. Plume, propri�taire
2 Cocuage de M. le P. Ubu
v Vidange
4 St Barbapoux, amant
4 St Memnon, vidangeur
4 Stes Miches, cat�chum�nes
4 Ste Lunette, solitaire
4 St Sphincter, prof�s
3 Sts Serpents d'Airain
4 Nativit� de St Donatien A. Fran�ois
4 St Woland, professeur
4 St Anal, cordelier et Ste Foire, anagogue
4 Ste F�tatoire, super
4 Ste Colombine, expurg�e
4 Ste Pyrotechnie, illumin�e
* Ontog�nie Pataphysique
3 Interpr�tation de L'Umour
4 Ste Purge, sage-femme
2 Apparition D'Ubu Roi
4 Ste Barbaque, na�ade
4 Sts Courts et Longs, gendarmes
4 St Raca, cagot
v D�faite du Mufle

3 Ste Bouzine, esprit
4 St Lucullus, amateur (Bloomsday)
4 Ste Dondon, amazone
4 Ste Tripe, r�publicaine
4 St Ugolin, mansuet
4 St Dieu, retrait�
4 St B�b� Toutout, �vang�liste
3 Ste Boudouille, bayad�re
4 Ste Outre, psychiatre
4 St Boudin, recteur
4 Sacre de Talou VII, empereur du Ponuk�l�
4 Ste Confiture, d�vote et Ste Cliche, donatrice
4 Sts Instintestins, conseillers intimes
4 St Colon, artilleur
3 Ste Giborgne, v�n�rable
4 St Inventaire, po�te
4 Ste Femelle, technicienne
2 Visitation de M�re Ubu
4 St Sein, tautologue
4 St P�rin�e, z�lateur
4 St Sp�culum, confesseur
2 F�te de Gidouille
4 St Ombilic, gymnosophiste
4 St Gris-gris, ventre
4 St Bouffre, pontife
4 Ste Goulache, odalisque
4 Ste Gandouse, hygi�niste
v Poche du P�re Ubu
2 Nom d'Ubu

1 F�te du P. Ubu (Ubu d'�t�)
4 Comm�moration du P. �b�
4 Ste Crapule, puriste et St Fantomas, archange
4 Ascension du Mouchard, statisticien, psychiatre et policier
4 St Arsouille, patricien
4 Sts Robot et Cornard, citoyens
4 St Biribi, taulier
2 Susception du Croc � Merdre
4 Sts �crase-Merdre, sectateurs
4 Sts Pieds Nickel�s, trinit�
4 Stes Canicule et Canule, jouvencelles
4 Sts Cannibales, philanthropes
4 St Dada, proph�te
4 Ste Anne, p�lerine, �nergum�ne
2 Procession aux Phynances
4 Transfiguration de St V. van Gogh, transmutateur
4 Ste Flamberge, voyante
4 St Trou, chauffeur
4 Ste Taloche, matrone
4 St Tiberge, fr�re qu�teur
4 Sts Catoblepas, lord et Anoblepas, amiral
2 Ubu �s Liens
4 St Pissembock, oncle
4 St Pissedoux, caporal des hommes libres
4 St Panurge, moraliste
4 St Gl�, neurologue-ali�niste
4 St Pistolet � Merdre, jubilaire
4 Nativit� de St Bruggle
v Le soleil solide froid

3 St Chibre, planton
4 Ste Ruth, z�latrice
4 St Zebb, passe-partout
4 St Mnester, confesseur
2 Assomption de Ste Messaline
v Penis Angelicus
4 St Patrobas, pompier
3 Ste L�da, ajusteuse
4 St Godemich�, �conome
4 Ste Nitouche, orante
4 Ste L�chefrite, botteuse
4 Ste Andouille, amphibologue
4 Ste Bitre, ouvreuse et St �talon, couvreur
3 Bataille de Morsang
3 Mort de Dionysos, surhomme
4 Nativit� de St Vibescu, poh�te et Comm�moration de Ste Cuculine d'Anc�ne
4 Ste Gallinac�e, cocotte
4 St Lingam, bouche-trou
4 St Pr�lote, capucin
4 St Pie VIII, navigant
3 St Erbrand, polytechnicien
2 Ste Dragonne, pyrophage
4 St Lazare, gare
4 Ste Orchid�e, aumoni�re
4 Nativit� apparente d'Artaud le Momo
4 Disparition de l'Ancien Breughel, incendiaire
4 St Priape, franc-tireur
3 Transfixion de Ste Messaline
v Le Term�s
EOF

1;
__END__

sub add_duration {
    my ($self, $dur) = @_;

    my $dm = $dur->delta_months;
    my $dd = $dur->delta_days;

    $self->{day}   += $dd;
    $self->{month} += _floor(($self->{day}-1)/29);
    $self->{day}    = ($self->{day}-1)%29 + 1;

    $self->{month} += $dm;
    $self->{year}  += _floor(($self->{year}-1)/13);
    $self->{month}  = ($self->{month}-1)%13 + 1;

    return $self;
}


1;
__END__

=head1 NAME

DateTime::Calendar::Pataphysical - Dates in the pataphysical calendar

=head1 SYNOPSIS

  use DateTime::Calendar::Pataphysical;

  $dt = DateTime::Calendar::Pataphysical->new( year  => 1752,
                                               month => 10,
                                               day   => 4 );

=head1 DESCRIPTION

DateTime::Calendar::Pataphysical is the implementation of the
pataphysical calendar. Each year in this calendar contains 13 months of
29 days. This regularity makes this a convenient alternative for the
irregular Gregorian calendar.

This module is designed to be easy to use in combination with
DateTime. Most of its methods correspond to a DateTime method of the
same name. 

=head1 METHODS

=over 4

=item * new( ... )

This class method accepts parameters for each date and time component:
"year", "month", "day".  Additionally, it accepts a "locale"
parameter.

The "rd_secs" parameter is also accepted. This parameter is only useful
in conversions to other calendars; this calendar does not use its value.

=item * from_epoch( epoch => $epoch, ... )

This class method can be used to construct a new object from
an epoch time instead of components.  Just as with the C<new()>
method, it accepts a "locale" parameter.

=item * now( ... )

This class method is equivalent to calling C<from_epoch()> with the
value returned from Perl's C<time()> function.

=item * from_object( object => $object, ... )

This class method can be used to construct a new object from
any object that implements the C<utc_rd_values()> method.  All
C<DateTime::Calendar> modules must implement this method in order to
provide cross-calendar compatibility.  This method accepts a
"locale" parameter.

The time part of $object is stored, and will only be used if the created
object is converted to another calendar. Only the date part of $object
is used to calculate the Pataphysical date. This calculation is based on
the local time and date of $object.

=item * last_day_of_month( ... )

This constructor takes the same arguments as can be given to the
C<now()> method, except for "day".  Additionally, both "year" and
"month" are required.

=item * clone

This object method returns a replica of the given object.

=item * year

Returns the year.

=item * month

Returns the month of the year, from 1..13.

=item * month_name

Returns the name of the current month.

=item * day_of_month, day, mday

Returns the day of the month, from 1..29.

=item * day_of_week, wday, dow

Returns the day of the week as a number, from 1..7, with 1 being
Sunday and 7 being Saturday.

=item * day_name

Returns the name of the current day of the week.

=item * day_of_year, doy

Returns the day of the year.

=item * ymd( $optional_separator ), date

=item * mdy( $optional_separator )

=item * dmy( $optional_separator )

Each method returns the year, month, and day, in the order indicated
by the method name.  Years are zero-padded to four digits.  Months and
days are 0-padded to two digits.

By default, the values are separated by a dash (-), but this can be
overridden by passing a value to the method.

=item * datetime

Equivalent to

  $dt->ymd('-') . 'EP'

=item * is_leap_year

This method returns a true or false indicating whether or not the
datetime object is in a leap year.

=item * week

 ($week_year, $week_number) = $dt->week

Returns information about the calendar week which contains this
datetime object. The values returned by this method are also available
separately through the week_year and week_number methods.

=item * week_year

Returns the year of the week. In the pataphysical calendar, this is
equal to the year of the date, as all weeks fall in one year only.

=item * week_number

Returns the week of the year, from 1..53.

The 29th of each month falls outside of any week; week_number returns
undef for these dates.

=item * utc_rd_values

Returns the current UTC Rata Die days and seconds as a two element
list.  This exists primarily to allow other calendar modules to create
objects based on the values provided by this object.

=item * utc_rd_as_seconds

Returns the current UTC Rata Die days and seconds purely as seconds.
This is useful when you need a single number to represent a date.

=item * strftime( $format, ... )

This method implements functionality similar to the C<strftime()>
method in C.  However, if given multiple format strings, then it will
return multiple elements, one for each format string.

See L<DateTime> for a list of all possible format specifiers. This
module implements all specifiers related to dates. There is one
additional specifier: C<%*> represents the feast of that date.

=item * feast

Returns the feast or vacuation of the given date.

=item * type_of_feast

Returns the type of feast or vacuation.

  '*' means F�te Supr�me Premi�re premi�re
  '1' means F�te Supr�me Premi�re seconde
  '2' means F�te Supr�me Seconde
  '3' means F�te Supr�me Tierce
  '4' means F�te Supr�me Quarte
  'v' means Vacuation

=item * is_imaginary

Returns true or false indicating whether the datetime object represents an
imaginary date.

=item * set( .. )

This method can be used to change the local components of a date time,
or its locale.  This method accepts any parameter allowed by the
C<new()> method.

=item * truncate( to => ... )

This method allows you to reset some of the local time components in
the object to their "zero" values.  The "to" parameter is used to
specify which values to truncate, and it may be one of "year",
"month", or "day".

=item * add_duration( $duration_object )

This method adds a C<DateTime::Duration> to the current datetime.  See
the L<DateTime::Duration|DateTime::Duration> docs for more detais.

=item * add( DateTime::Duration->new parameters )

This method is syntactic sugar around the C<add_duration()> method.  It
simply creates a new C<DateTime::Duration> object using the parameters
given, and then calls the C<add_duration()> method.

=item * subtract_duration( $duration_object )

When given a C<DateTime::Duration> object, this method simply calls
C<invert()> on that object and passes that new duration to the
C<add_duration> method.

=item * subtract( DateTime::Duration->new parameters )

Like C<add()>, this is syntactic sugar for the C<subtract_duration()>
method.

=item * subtract_datetime( $datetime )

This method returns a new C<DateTime::Duration> object representing
the difference between the two dates.

=item * compare

  $cmp = DateTime->compare($dt1, $dt2);

  @dates = sort { DateTime->compare($a, $b) } @dates;

Compare two DateTime objects.  The semantics are compatible with
Perl's C<sort()> function; it returns -1 if $a < $b, 0 if $a == $b, 1
if $a > $b.

Of course, since DateTime objects overload comparison operators, you
can just do this anyway:

  @dates = sort @dates;

=back

=head1 BUGS

=over 4

=item * Adding a week to a date is exactly equivalent to adding seven days
in this module because of the way DateTime::Duration is implemented.
The Hunyadis are not taken into account.

=item * from_epoch() and now() probably only work on Unix.

=back

=head1 SUPPORT

Support for this module is provided via the datetime@perl.org email
list. See http://lists.perl.org/ for more details.

=head1 AUTHOR

Eugene van der Pijll <pijll@gmx.net>

=head1 COPYRIGHT

Copyright (c) 2003, 2004 Eugene van der Pijll.  All rights reserved.
This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=head1 SEE ALSO

L<DateTime>

datetime@perl.org mailing list

=cut
