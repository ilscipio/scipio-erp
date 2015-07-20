<#--<@nav type="magellan">
    <@mli arrival="breadcrumbs"><a href="#breadcrumbs">Breadcrumbs</a></@mli>
    <@mli arrival="grid"><a href="#grid">Grid</a></@mli>
    <@mli arrival="blockgrid"><a href="#blockgrid">Block-Grid</a></@mli>
    <@mli arrival="buttons"><a href="#buttons">Buttons</a></@mli>
    <@mli arrival="panel"><a href="#panel">Panel</a></@mli>
</@nav>-->

<div ${mtarget("breadcrumbs")} id="breadcrumbs"></div>
<@nav type="breadcrumbs">
    <li><a href="#">Home</a></li>
    <li><a href="#">Features</a></li>
    <li class="${style_nav_breadcrumbs_disabled!}"><a href="#">Gene Splicing</a></li>
    <li class="${style_nav_breadcrumbs_active!}"><a href="#">Cloning</a></li>
</@nav>

<@section>
    <h2 ${mtarget("grid")} id="grid">Grid</h2>
    <@row class="${style_grid_display!}">
        <@cell columns=2>2</@cell>
        <@cell columns=4>4</@cell>
        <@cell columns=6>6</@cell>
    </@row>
    <@row class="${style_grid_display!}">
        <@cell columns=3>3</@cell>
        <@cell columns=6>6</@cell>
        <@cell columns=3>3</@cell>
    </@row>
    <@row class="${style_grid_display!}">
        <@cell columns=2>2</@cell>
        <@cell columns=8>8</@cell>
        <@cell columns=2>2</@cell>
    </@row>   
    <@row class="${style_grid_display!}">
        <@cell columns=3>3</@cell>
        <@cell columns=9>9</@cell>
    </@row>   
    <@row class="${style_grid_display!}">
        <@cell columns=4>4</@cell>
        <@cell columns=8>8</@cell>
    </@row>       
    <@row class="${style_grid_display!}">
        <@cell columns=6>6</@cell>
        <@cell columns=6>6</@cell>
    </@row>
</@section>

<@section>
    <h2 ${mtarget("blockgrid")} id="blockgrid">Tiles</h2>
    <@grid type="tiles">
        <@tile type="large" color=3 icon="fi-compass">My money's in that office, right? If she start giving me some bullshit about it ain't there, and we got to go someplace else and get it, I'm gonna shoot you in the head then and there. Then I'm gonna shoot that bitch in the kneecaps, find out where my goddamn money is. She gonna tell me too. Hey, look at me when I'm talking to you, motherfucker. You listen: we go in there, and that nigga Winston or anybody else is in there, you the first motherfucker to get shot. You understand?</@tile>
        <@tile type="normal" color=7 title="Test"></@tile>
        <@tile type="small" color=6 title="" icon="fi-flag"></@tile>
        <@tile type="small" color=3 title="Test"></@tile>
        <@tile type="wide" color=0 title="dasdsadsas dasdas"></@tile>
        <@tile type="large" color=6 title="Test" icon="fi-star"></@tile>
        <@tile type="wide" color=2 title="Test 3" icon="fi-heart">My money's in that office, right? If she start giving me some bullshit about it ain't there, and we got to go someplace else and get it, I'm gonna shoot you in the head then and there. Then I'm gonna shoot that bitch in the kneecaps, find out where my goddamn money is. She gonna tell me too. Hey, look at me when I'm talking to you, motherfucker. You listen: we go in there, and that nigga Winston or anybody else is in there, you the first motherfucker to get shot. You understand?</@tile>    
        <@tile type="normal" color=2 icon="fi-layout">My money's in that office, right? If she start giving me some bullshit about it ain't there, and we got to go someplace else and get it, I'm gonna shoot you in the head then and there. Then I'm gonna shoot that bitch in the kneecaps, find out where my goddamn money is. She gonna tell me too. Hey, look at me when I'm talking to you, motherfucker. You listen: we go in there, and that nigga Winston or anybody else is in there, you the first motherfucker to get shot. You understand?</@tile>
        <@tile type="normal" color=5 title="2" icon="fi-music"></@tile>
        <@tile type="small" color=4 title="Test"></@tile>
        <@tile type="small" color=6 title="Test"></@tile>
        <@tile type="small" color=4 title="Test"></@tile>
        <@tile type="small" color=5 title="Test"></@tile>
        <@tile type="normal" color=0></@tile>
        <@tile type="normal" color=1 title="2" icon="fi-like"></@tile>
    </@grid>
</@section>

<@section>
<h2 ${mtarget("buttons")} id="buttons">Buttons</h2>
<h3>Heading</h3>
<h1>h1.<h1>
<h2>h2.<h2>
<h3>h3.<h3>
<h4>h4.<h4>
<h5>h5.<h5>
<h6>h6.<h6>

<h3>Shapes</h3>
<a href="#" class="${style_button!} ${style_tiny!}">Tiny Button</a>
<a href="#" class="${style_button!} ${style_small!}">Small Button</a>
<a href="#" class="${style_button!}">Default Button</a>
<a href="#" class="${style_button!} ${style_disabled!}">Disabled Button</a>
<a href="#" class="${style_button!} ${style_large!}">Large Button</a>
<a href="#" class="${style_button!} ${style_expand!}">Expanded Button</a>
<a href="#" class="button round">Round Button</a>
<a href="#" class="button radius">Radius Button</a>

<h3>Colors</h3>
<a href="#" class="${style_button!}">Default Button</a>
<a href="#" class="${style_button!} ${style_color_green!}">Success Button</a>
<a href="#" class="${style_button!} ${style_color_grey!}">Secondary Button</a>
<a href="#" class="${style_button!} ${style_color_red!}">Alert Button</a>
<a href="#" class="${style_button!} ${style_color_info!}">Info Button</a>
<a href="#" class="${style_button!} ${style_disabled}">Disabled Button</a>

<h3>Button Groups</h3>
<ul class="${style_button_group} ${style_button_force!}">
    <li><a href="#" class="${style_button!} ${style_small!}">Button 1</a></li>
    <li><a href="#" class="${style_button!} ${style_small!} ${style_disabled}">Button 2</a></li>
    <li><a href="#" class="${style_button!} ${style_small!} ${style_color_green}">Button 3</a></li>
</ul>

<ul class="${style_button_group} ${style_button_force!}">
    <li><a href="#" class="${style_button!}">Button 1</a></li>
    <li><a href="#" class="${style_button!}">Button 2</a></li>
    <li><a href="#" class="${style_button!}">Button 3</a></li>
</ul>
</@section>                                     

<@section>
    <h2 ${mtarget("panel")} id="panel">Panel</h2>
    <@row>
        <@cell columns=6>
            <@panel>
               Run a manual sweep of anomalous airborne or electromagnetic readings. Radiation levels in our atmosphere have increased by 3,000 percent. Electromagnetic and subspace wave fronts approaching synchronization. What is the strength of the ship's deflector shields at maximum output? The wormhole's size and short period would make this a local phenomenon. Do you have sufficient data to compile a holographic simulation?
            </@panel>
        </@cell>
        <@cell columns=6>
            <@panel type="callout">
                I have reset the sensors to scan for frequencies outside the usual range. By emitting harmonic vibrations to shatter the lattices. We will monitor and adjust the frequency of the resonators. He has this ability of instantly interpreting and extrapolating any verbal communication he hears. It may be due to the envelope over the structure, causing hydrogen-carbon helix patterns throughout. I'm comparing the molecular integrity of that bubble against our phasers.
            </@panel>
        </@cell>
    </@row>
</@section>

<@section>
    <h2 ${mtarget("charts")} id="charts">Charts</h2>
    <@row>
        <@cell columns="4">
            <@chart type="pie">
                <@chartdata value="36" title="Peperoni"/>
                <@chartdata value="2" title="Sausage"/> 
                <@chartdata value="19" title="Cheese"/> 
                <@chartdata value="6" title="Chicken"/> 
                <@chartdata value="27" title="Other"/>  
            </@chart>
        </@cell>
        
        <@cell columns="4">
            <@chart type="bar">
                <@chartdata value="36" title="Peperoni"/>
                <@chartdata value="14" title="Sausage"/> 
                <@chartdata value="8" title="Cheese"/> 
                <@chartdata value="11" title="Chicken"/> 
                <@chartdata value="7" title="Other"/>  
            </@chart>
        </@cell>
        
        <@cell columns="4">
            <@chart type="line">
                <@chartdata value="36" value2="1" title="Peperoni"/>
                <@chartdata value="2" value2="2" title="Sausage"/> 
                <@chartdata value="19" value2="3" title="Cheese"/> 
                <@chartdata value="6" value2="4" title="Chicken"/> 
                <@chartdata value="27" value2="5" title="Other"/>  
            </@chart>
        </@cell>
    </@row>
</@section>

<@section>
    <h2 ${mtarget("misc")} id="misc">Misc</h2>
    <@progress value=60/>
</@section>