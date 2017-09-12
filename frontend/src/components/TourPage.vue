<template>
  <div class="tour-index"> 
    <router-view></router-view>
  </div>
</template>

<script>
import axios from 'axios';

export default {
  name: "tour-page",
  props: ["name", "tour"],
  data () {
    return {
    }
  }
}
</script>

<style scoped>
</style>
